;;; goonix: mixed package policy and builders embedded at lowering.
(use-modules (guix)
             (guix tests)
             (guix ca)
             (guix ca policy)
             (guix sets)
             (guix derivations)
             (guix gexp)
             (guix build-system trivial)
             (gnu packages base)
             (gnu packages bootstrap)
             (ice-9 binary-ports)
             (ice-9 textual-ports)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "ca-policy")
(define store (open-connection-for-tests))
(%graft? #f)

(define (addressing drv)
  (assoc-ref (derivation-builder-environment-vars drv) "goonixAddressing"))

(define (bytes drv)
  (call-with-input-file (derivation-file-name drv) get-bytevector-all #:binary #t))

(define upstream-hello (package-derivation store hello))
(define hello-bytes (bytes upstream-hello))
(define inherited-hello (package (inherit hello)))

(define (mixed store package)
  (parameterize ((%package-addressing-policy 'mixed))
    (package-derivation store package)))

(define (closure-alist table)
  (sort (hash-map->list cons table)
        (lambda (left right) (string<? (car left) (car right)))))


(define (derivation-closure drv)
  (let ((table (make-hash-table)))
    (let visit ((d drv))
      (unless (hash-ref table (derivation-file-name d))
        (hash-set! table (derivation-file-name d)
                   (call-with-input-file (derivation-file-name d)
                     get-bytevector-all))
        (for-each (lambda (input)
                    (visit (derivation-input-derivation input)))
                  (derivation-inputs d))))
    table))

(define (dependency-closure drv)
  (let ((table (derivation-closure drv)))
    (hash-remove! table (derivation-file-name drv))
    table))

(test-group "the default origin marker recognizes upstream definitions"
  (test-assert "hello is upstream" (upstream-package? hello))
  (test-assert "an inherited package in tests is not upstream"
    (not (upstream-package? inherited-hello)))
  (test-assert "a runtime bootstrap package retains its upstream origin"
    (upstream-package? %bootstrap-coreutils&co)))

(test-group "fresh mixed lowering preserves the upstream graph"
  (let ((reference (open-connection-for-tests)))
    (let ((input (package-derivation reference hello)))
      (close-connection reference)
      (let ((fresh (open-connection-for-tests)))
        (let ((actual (mixed fresh hello)))
          (close-connection fresh)
          (test-equal "fresh mixed hello derivation is byte-identical"
            hello-bytes (bytes actual))
          (test-equal "every hello dependency is byte-identical"
            (closure-alist (derivation-closure input))
            (closure-alist (derivation-closure actual))))))))

(test-group "fresh mixed inherited hello is content-addressed"
  (let ((fresh (open-connection-for-tests)))
    (let ((actual (mixed fresh inherited-hello)))
      (close-connection fresh)
      (test-equal "inherited hello is content-addressed without selection"
        "content" (addressing actual))
      (test-assert "inherited hello differs from upstream hello"
        (not (string=? (derivation-file-name upstream-hello)
                       (derivation-file-name actual))))
      (let* ((dependent
              (package (inherit inherited-hello)
                (name "ca-policy-unselected-dependent")
                (inputs (list (list "hello" inherited-hello)))))
             (second (open-connection-for-tests))
             (dependent-drv (mixed second dependent)))
        (close-connection second)
        (test-equal "an unselected dependent is content-addressed"
          "content" (addressing dependent-drv))))))

(test-group "shared origins and all their dependencies are order independent"
  (let* ((snippet
          (package (inherit hello) (name "ca-policy-upstream-snippet")
            (source (origin (inherit (package-source hello))
                            (snippet #~(begin #t))))))
         (inherited (package (inherit snippet) (name "ca-policy-local-snippet")))
         (marked? (let ((default (%upstream-package?)))
                    (lambda (package)
                      (or (eq? package snippet) (default package)))))
         (mixed-marked (lambda (connection package)
                         (parameterize ((%package-addressing-policy 'mixed)
                                        (%upstream-package? marked?))
                           (package-derivation connection package)))))
    (for-each
     (lambda (packages)
       (let ((connection (open-connection-for-tests)))
         (parameterize ((%package-addressing-policy 'mixed)
                        (%upstream-package? marked?))
           (for-each (lambda (package) (package-derivation connection package))
                     packages))
         (close-connection connection))
       (test-assert "mixed shared-origin lowering completes in either order" #t))
     (list (list snippet inherited) (list inherited snippet)))
    (let* ((upstream-reference (derivation-closure (package-derivation store snippet)))
           (upstream-mixed
            (let ((connection (open-connection-for-tests)))
              (let ((result (derivation-closure (mixed-marked connection snippet))))
                (close-connection connection) result)))
           (local-drv
            (let ((connection (open-connection-for-tests)))
              (let ((result (mixed-marked connection inherited)))
                (close-connection connection) result))))
      (test-equal "upstream snippet package's whole graph is byte-identical"
        (closure-alist upstream-reference) (closure-alist upstream-mixed))
      (test-equal "local snippet package is content-addressed"
        "content" (addressing local-drv))
      (test-equal "local snippet package's upstream dependencies are byte-identical"
        (closure-alist (dependency-closure (package-derivation store inherited)))
        (closure-alist (dependency-closure local-drv))))))

(test-group "the addressing cache includes the current mode on every call"
  (let ((input (parameterize ((%package-addressing-policy 'input)
                              (%upstream-package? (upstream-package-set (set))))
                 (package-derivation store inherited-hello)))
        (content (parameterize ((%package-addressing-policy 'content)
                                (%upstream-package? (upstream-package-set (set))))
                   (package-derivation store inherited-hello))))
    (test-assert "the same package lowered under input then content differs"
      (and (not (addressing input))
           (string=? "content" (addressing content))
           (not (string=? (derivation-file-name input)
                          (derivation-file-name content)))))))

(define* (trivial name builder #:optional (inputs '()) (properties '()))
  (package
    (name name)
    (version "1")
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:guile %bootstrap-guile #:builder builder))
    (inputs inputs)
    (properties properties)
    (synopsis "CA policy test")
    (description "CA policy test")
    (home-page #f)
    (license #f)))

(test-group "mixed uses the graph for the requested system and target"
  (let* ((opaque (trivial "ca-policy-marked-opaque" #~(mkdir #$output)
                          '() '((self-references . opaque))))
         (on-opaque (trivial "ca-policy-on-opaque" #~(symlink #$opaque #$output)
                             `(("opaque" ,opaque))))
         (foreign (trivial "ca-policy-foreign" #~(mkdir #$output)))
         (system-package
          (package (inherit (trivial "ca-policy-system" #~(mkdir #$output)))
            (inputs (if (string=? (%current-system) "i686-linux")
                        `(("foreign" ,foreign)) '()))))
         (target-package
          (package (inherit (trivial "ca-policy-target" #~(mkdir #$output)))
            (inputs (if (%current-target-system) `(("foreign" ,foreign)) '())))))
    (parameterize ((%package-addressing-policy 'mixed)
                   (%upstream-package?
                    (upstream-package-set
                     (list->setq (list opaque on-opaque system-package
                                       target-package %bootstrap-guile)))))
      (test-equal "upstream dependent on opaque input is content-addressed"
        "content" (addressing (package-derivation store on-opaque)))
      (test-equal "i686-specific foreign input promotes its parent"
        "content" (addressing (package-derivation store system-package
                                                   "i686-linux")))
      (test-equal "cross-target foreign input promotes its parent"
        "content" (addressing (package-cross-derivation
                               store target-package "aarch64-linux-gnu"))))))

(define a
  (trivial "ca-policy-a"
           #~(call-with-output-file #$output
               (lambda (port) (display "content from A\n" port)))))

(define b
  (trivial "ca-policy-b"
           #~(call-with-output-file #$output
               (lambda (port) (display #$a port)))
           `(("a" ,a))))

(define opaque-a
  (trivial "ca-policy-opaque-a"
           #~(call-with-output-file #$output
               (lambda (port) (display "resolved A\n" port)))
           '() '((self-references . opaque))))

(define opaque-b
  (trivial "ca-policy-opaque-b"
           #~(call-with-output-file #$output
               (lambda (port) (display #$opaque-a port)))
           `(("opaque-a" ,opaque-a))))

(define (build-and-read package)
  (let* ((fresh (open-connection-for-tests))
         (drv (package-derivation fresh package)))
    (close-connection fresh)
    (build-derivations store (list drv))
    (let* ((out (realised-output-path store drv "out"))
           (text (call-with-input-file out get-string-all)))
      (test-assert (string-append (package-name package) " embeds its builder")
        (and (null? (derivation-sources drv))
             (assoc-ref (derivation-builder-environment-vars drv) "__files")
             (string-prefix? "$NIX_BUILD_TOP/"
                             (last (derivation-builder-arguments drv)))))
      (test-assert (string-append (package-name package) " has no placeholder in its output")
        (not (placeholder-path? text)))
      (list drv out text))))

(test-group "a computed file is promoted by its actual content-addressed input"
  (let ((connection (open-connection-for-tests)))
    (let ((b-drv
           (parameterize ((%package-addressing-policy 'mixed))
             (package-derivation connection b)))
          (computed
           (parameterize ((%package-addressing-policy 'mixed))
             (run-with-store connection
               (lower-object
                (computed-file "ca-policy-computed"
                               #~(call-with-output-file #$output
                                   (lambda (port) (display #$b port)))
                               #:guile %bootstrap-guile))))))
      (close-connection connection)
      (test-equal "computed-file test starts with a content-addressed B"
        "content" (addressing b-drv))
      (test-equal "computed file is content-addressed, not input-addressed"
        "content" (addressing computed))
      (test-assert "computed file embeds its builder"
        (assoc-ref (derivation-builder-environment-vars computed) "__files")))))

(test-group "real builds use the realised input path"
  (parameterize ((%package-addressing-policy 'mixed)
                 (%upstream-package? (upstream-package-set (list->setq (list %bootstrap-guile)))))
    (let* ((a-result (build-and-read a))
           (b-result (build-and-read b)))
      (test-equal "A is content-addressed by the mixed rule"
        "content" (addressing (first a-result)))
      (test-equal "B is content-addressed by the mixed rule"
        "content" (addressing (first b-result)))
      (test-equal "trivial gexp B writes A's realised path"
        (second a-result) (third b-result))
      (test-assert "B refers to A's realised path"
        (member (second a-result) (references store (second b-result)))))
    (let* ((a-result (build-and-read opaque-a))
           (b-result (build-and-read opaque-b)))
      (test-equal "the same works with A marked opaque"
        (second a-result) (third b-result))
      (test-equal "opaque A uses resolved addressing"
        "resolved" (addressing (first a-result)))
      (test-assert "opaque B refers to A's realised path"
        (member (second a-result) (references store (second b-result)))))))

(close-connection store)
(test-end)
