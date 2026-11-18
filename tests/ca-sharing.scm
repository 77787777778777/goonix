;;; goonix: adversarial tests of record sharing/selection (CA-5), collector
;;; reachability (plan 10) and mixed-policy safety (CA-4).  These exercise the
;;; store-side guarantees end-to-end against the test daemon: facts survive
;;; collection, choices are authoritative and repaired, disagreements are
;;; stored as second records, and an input-addressed derivation may not depend
;;; on a content-addressed one.
(use-modules (guix)
             (guix tests)
             (guix ca)
             (guix store)
             (guix serialization)
             (guix base16)
             (gcrypt hash)
             (gnu packages bootstrap)
             (guix derivations)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-34)
             (srfi srfi-64)
             (rnrs bytevectors)
             (rnrs io ports))

(test-begin "ca-sharing")
(define store (open-connection-for-tests))
(%graft? #f)
(define guile-drv (package-derivation store %bootstrap-guile))
(build-derivations store (list guile-drv))
(define bootstrap-guile (string-append (derivation->output-path guile-drv)
                                       "/bin/guile"))

;; Build a content/resolved/input-addressed derivation whose builder is the
;; bootstrap Guile running SCRIPT.  Inputs are passed as <derivation-input>.
(define* (ca-drv name script #:key (inputs '()) (addressing 'content)
                 (env-vars '()))
  (derivation store name bootstrap-guile
              `("--no-auto-compile" "-c" ,script)
              #:inputs (cons (derivation-input guile-drv)
                             (map derivation-input inputs))
              #:addressing addressing
              #:env-vars env-vars))

(define (write-output text)
  (string-append "(call-with-output-file (getenv \"out\") (lambda (p) (display "
                 (object->string text) " p)))"))

(define (read-all file) (call-with-input-file file get-string-all))
(define (realised drv) (realised-output-path store drv "out"))

;; The resolved derivation is the deriver recorded on the realised output.
(define (resolved-derivation drv)
  (path-info-deriver (query-path-info store (realised drv))))

(define (resolved-hash drv)
  (bytevector->base16-string
   (sha256 (call-with-input-file (resolved-derivation drv) get-bytevector-all))))

(define (build-records hash output)
  ((@ (guix ca) build-records) store (base16-string->bytevector hash) output))

(define (record-paths hash output)
  (map (lambda (r) (list-ref r 2)) (build-records hash output)))

(test-assert "record query leaves the daemon connection synchronized"
  (let* ((drv (ca-drv "rec-sync" (write-output "sync")))
         (out (begin (build-derivations store (list drv)) (realised drv)))
         (records (build-records (resolved-hash drv) "out")))
    (and (= 1 (length records))
         (string=? out (list-ref (car records) 2))
         (valid-path? store out))))

(test-assert "a local build writes a signed record naming the realised path"
  (let* ((drv (ca-drv "rec-signed" (write-output "signed")))
         (out (begin (build-derivations store (list drv)) (realised drv)))
         (records (build-records (resolved-hash drv) "out")))
    (and (= 1 (length records))
         (string=? out (list-ref (car records) 2))
         (string=? "local" (list-ref (car records) 3))
         (not (string-null? (list-ref (car records) 5))))))

(test-assert "two derivations with one result share one path and one record"
  (let* ((one (ca-drv "rec-dedup" (write-output "same")
                      #:env-vars '(("unused" . "one"))))
         (two (ca-drv "rec-dedup" (write-output "same")
                      #:env-vars '(("unused" . "two")))))
    (build-derivations store (list one))
    (build-derivations store (list two))
    (and (string=? (realised one) (realised two))
         ;; The environment entry remains in each resolved derivation.  Check
         ;; the second one's record without claiming that the two R are equal.
         (= 1 (length (build-records (resolved-hash two) "out"))))))

(test-assert "the choice is authoritative across rebuilds"
  (let* ((drv (ca-drv "rec-choice" (write-output "chosen")))
         (out (begin (build-derivations store (list drv)) (realised drv))))
    (build-derivations store (list drv))
    (string=? out (realised drv))))

(test-assert "records and choices survive collection of the output"
  (let* ((drv (ca-drv "rec-survive" (write-output "survive")))
         (out (begin (build-derivations store (list drv)) (realised drv)))
         (hash (resolved-hash drv)))
    (delete-paths store (list out))
    (and (not (valid-path? store out))
         ;; The fact is permanent: it still names the collected path.
         (member out (record-paths hash "out")))))

(test-assert "a collected choice is repaired from the record on rebuild"
  (let* ((one (ca-drv "rec-repair" (write-output "repair")
                      #:env-vars '(("unused" . "one"))))
         (two (ca-drv "rec-repair" (write-output "repair")
                      #:env-vars '(("unused" . "two"))))
         (out (begin (build-derivations store (list one)) (realised one))))
    (delete-paths store (list out))
    (build-derivations store (list two))
    (and (string=? out (realised two))
         ;; The original derivation's stale choice is repaired too.
         (string=? out (realised one))
         (string=? "repair" (read-all out)))))

(test-assert "gc-keep-outputs keeps an output whose derivation is rooted"
  (let* ((drv (ca-drv "rec-keepout" (write-output "keepout")))
         (out (begin (build-derivations store (list drv)) (realised drv)))
         (drvfile (derivation-file-name drv)))
    ;; Enable keep-outputs for this connection, root the original derivation.
    ((@@ (guix store) set-options) store
     #:keep-failed? #f #:keep-going? #f #:fallback? #f #:verbosity 0
     #:offload? #f #:build-verbosity 0 #:log-type 0 #:print-build-trace #t
     #:use-substitutes? #f #:settings '(("gc-keep-outputs" . "true")))
    (add-permanent-root drvfile)
    (let ((alive (member out (live-paths store))))
      (remove-permanent-root drvfile)
      ;; Restore the default so later tests are not affected.
      ((@@ (guix store) set-options) store
       #:keep-failed? #f #:keep-going? #f #:fallback? #f #:verbosity 0
       #:offload? #f #:build-verbosity 0 #:log-type 0 #:print-build-trace #t
       #:use-substitutes? #f #:settings '(("gc-keep-outputs" . "false")))
      (->bool alive))))

(test-assert "gc-keep-outputs keeps an output via its record deriver"
  (let* ((drv (ca-drv "rec-keeprec" (write-output "keeprec")))
         (out (begin (build-derivations store (list drv)) (realised drv)))
         (resolved (resolved-derivation drv)))
    ((@@ (guix store) set-options) store
     #:keep-failed? #f #:keep-going? #f #:fallback? #f #:verbosity 0
     #:offload? #f #:build-verbosity 0 #:log-type 0 #:print-build-trace #t
     #:use-substitutes? #f #:settings '(("gc-keep-outputs" . "true")))
    ;; Root ONLY the resolved derivation; the output is kept via the record.
    (add-permanent-root resolved)
    (let ((alive (member out (live-paths store))))
      (remove-permanent-root resolved)
      ((@@ (guix store) set-options) store
       #:keep-failed? #f #:keep-going? #f #:fallback? #f #:verbosity 0
       #:offload? #f #:build-verbosity 0 #:log-type 0 #:print-build-trace #t
       #:use-substitutes? #f #:settings '(("gc-keep-outputs" . "false")))
      (->bool alive))))

(test-assert "--check preserves a self-referencing result and its choice"
  (let* ((drv (ca-drv "rec-check-self"
                      (string-append
                       "(call-with-output-file (getenv \"out\") "
                       "(lambda (p) (display (getenv \"out\") p)))")))
         (out (begin (build-derivations store (list drv)) (realised drv)))
         (hash (resolved-hash drv)))
    (build-derivations store (list drv) (build-mode check))
    (and (string=? out (read-all out))
         (string=? out (realised drv))
         (valid-path? store out)
         ;; A check is a distinct fact (origin local-check), even when it
         ;; reaches the same address.  Reproducibility means one result path.
         (equal? (list out) (delete-duplicates (record-paths hash "out")))
         (= 2 (length (build-records hash "out")))
         (verify-store store #:check-contents? #t #:repair? #f))))

(test-assert "--check stores a second record for a non-reproducible result"
  (let* ((drv (ca-drv "rec-nonrepro"
                      "(call-with-output-file (getenv \"out\") (lambda (p) (display (gettimeofday) p)))"))
         (out (begin (build-derivations store (list drv)) (realised drv)))
         (hash (resolved-hash drv)))
    (sleep 2)                          ; ensure a different timestamp
    (guard (c ((store-protocol-error? c) #t))
      (build-derivations store (list drv) (build-mode check)))
    (let ((paths (delete-duplicates (record-paths hash "out"))))
      (and (>= (length paths) 2)
           ;; The choice is unchanged by a check.
           (string=? out (realised drv))
           (member out paths)))))

(test-assert "the two-glibc scenario ends in one coherent closure"
  (let* ((g1 (ca-drv "glibc" (write-output "GLIBC")))
         (g2 (ca-drv "glibc" (write-output "GLIBC")
                     #:env-vars '(("variant" . "two"))))
         (pkg-script
          ;; Embed the resolved dependency path so the output carries a real
          ;; runtime reference to the glibc item (that is what a closure is).
          "(use-modules (ice-9 rdelim)) (call-with-output-file (getenv \"out\") (lambda (p) (display (read-delimited \"\" (open-input-file (getenv \"dep\"))) p) (display \"-\" p) (display (getenv \"label\") p) (display \" ref:\" p) (display (getenv \"dep\") p)))")
         (pkg (lambda (name g label)
                (derivation store name bootstrap-guile
                            `("--no-auto-compile" "-c" ,pkg-script)
                            #:inputs (list (derivation-input guile-drv)
                                           (derivation-input g))
                            #:addressing 'content
                            #:env-vars `(("dep" . ,(derivation->output-path g))
                                         ("label" . ,label)))))
         (p1 (pkg "pkg1" g1 "P1"))
         (p2 (pkg "pkg2" g2 "P2")))
    (build-derivations store (list g1 g2))
    (build-derivations store (list p1 p2))
    (let* ((o1 (realised p1))
           (o2 (realised p2))
           (glibc-items (lambda (o)
                          (filter (lambda (p) (string-contains p "glibc"))
                                  (requisites store (list o))))))
      (and (string=? (realised g1) (realised g2))
           (string-prefix? "GLIBC-P1" (read-all o1))
           (string-prefix? "GLIBC-P2" (read-all o2))
           (= 1 (length (glibc-items o1)))
           (= 1 (length (glibc-items o2)))
           (equal? (glibc-items o1) (glibc-items o2))))))

(test-assert "an input-addressed derivation may not depend on a CA input"
  (let ((ca-input (ca-drv "ca-for-mixed" (write-output "X"))))
    ;; Registering an input-addressed derivation with a CA input must be
    ;; refused by the mixed policy (plan 3.1), before any build.
    (guard (c ((store-protocol-error? c)
               (string-contains (store-protocol-error-message c)
                                "mixed policy")))
      (derivation store "ia-mixed" bootstrap-guile
                  '("--no-auto-compile" "-c"
                    "(call-with-output-file (getenv \"out\") (lambda (p) (display \"Y\" p)))")
                  #:inputs (list (derivation-input ca-input))
                  #:addressing 'input)
      #f)))

(test-assert "input-addressed derivations stay byte-identical and buildable"
  (let* ((ia (ca-drv "ia-compat" (write-output "legacy") #:addressing 'input))
         (env (derivation-builder-environment-vars ia)))
    (and (not (assoc "goonixAddressing" env))
         (not (placeholder-path? (derivation->output-path ia)))
         (not (token-store-path? (derivation->output-path ia)))
         (build-derivations store (list ia))
         (string=? "legacy" (read-all (derivation->output-path ia))))))

(close-connection store)
(test-end)