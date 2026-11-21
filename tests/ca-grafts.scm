;;; goonix: CA-6 gate — grafts as content-addressed derivations.
;;;
;;; A graft that changes nothing yields the original path; a real
;;; replacement yields a different path and rewrites the reference.  The
;;; graft derivation is content-addressed (mixed policy), so it writes its
;;; own output at a self-token; the rewriter accepts that token as a
;;; replacement (guix/build/graft.scm, #:own-outputs) and the daemon
;;; finalises it to the content address.
(use-modules (guix)
             (guix tests)
             (guix grafts)
             (guix gexp)
             (guix ca policy)
             (guix derivations)
             (guix monads)
             (gnu packages bootstrap)
             (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "ca-grafts")

(define store (open-connection-for-tests))
(%graft? #f)
(%graft-with-utf8-locale? #f)

(define guile-drv (package-derivation store %bootstrap-guile))
(build-derivations store (list guile-drv))

(define (fresh-name prefix)
  (string-append prefix "-"
                 (number->string (random 1000000000
                                         (random-state-from-platform)))))

(define (realised drv) (realised-output-path store drv "out"))
(define (read-all file) (call-with-input-file file get-string-all))

;; A content-addressed derivation whose output is TEXT.
(define (plain-drv name text)
  (run-with-store store
    (gexp->derivation name
      #~(call-with-output-file #$output (lambda (p) (display #$text p)))
      #:addressing 'content
      #:guile-for-build guile-drv)))

;; A content-addressed derivation whose output references DEP-PATH.
(define (ref-drv name dep-path text)
  (run-with-store store
    (gexp->derivation name
      #~(call-with-output-file #$output
          (lambda (p) (format p "~a/~a" #$dep-path #$text)))
      #:addressing 'content
      #:guile-for-build guile-drv)))

;; Graft the DEP reference inside ORIG: origin DEP, replacement REPL.
(define (graft-dep orig dep repl)
  (run-with-store store
    (graft-derivation/shallow orig
      (list (graft (origin dep) (replacement repl)))
      #:guile guile-drv)))

;; The gate runs under mixed policy, the plan's first policy: the graft is
;; content-addressed (its inputs are), while the graft's module-import
;; compilation stays input-addressed.
(parameterize ((%package-addressing-policy 'mixed))
  (let* ((dep-name  (fresh-name "ca-graft-dep"))
         (orig-name (fresh-name "ca-graft-orig"))
         (dep       (plain-drv dep-name "DEP"))
         (_         (build-derivations store (list dep)))
         (dep-path  (realised dep))
         (orig      (ref-drv orig-name dep-path "hello"))
         (_         (build-derivations store (list orig)))
         (orig-path (realised orig)))

    (test-equal "a graft that changes nothing yields the original path"
      orig-path
      (let* ((noop    (plain-drv dep-name "DEP"))
             (_       (build-derivations store (list noop)))
             (grafted (graft-dep orig dep noop))
             (_       (build-derivations store (list grafted))))
        (realised grafted)))

    (test-assert "a real replacement yields a different path and rewrites the reference"
      (let* ((real     (plain-drv dep-name "OTHER"))
             (_        (build-derivations store (list real)))
             (real-path (realised real))
             (grafted  (graft-dep orig dep real))
             (_        (build-derivations store (list grafted)))
             (path     (realised grafted)))
        (and (not (string=? orig-path path))
             ;; The dep reference is rewritten to the replacement's path;
             ;; orig's own text ("hello") is preserved.
             (string=? (string-append real-path "/hello")
                       (read-all path)))))))

(close-connection store)
(test-end)