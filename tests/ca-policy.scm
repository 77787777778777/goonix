;;; goonix: CA-4 Guix-on-top policy and embedded builder tests.
(use-modules (guix)
             (guix tests)
             (guix ca)
             (guix ca policy)
             (guix derivations)
             (guix gexp)
             (gnu packages base)
             (gnu packages bootstrap)
             (ice-9 textual-ports)
             (ice-9 binary-ports)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "ca-policy")
(define store (open-connection-for-tests))
(%graft? #f)

(define (addressing drv)
  (assoc-ref (derivation-builder-environment-vars drv) "goonixAddressing"))

(define (recipe-fields drv)
  (list (derivation-name drv)
        (derivation-builder drv)
        (derivation-builder-arguments drv)
        (derivation-system drv)
        (map derivation-input-path (derivation-inputs drv))
        (derivation-sources drv)
        (derivation-output-names drv)))

(define original (package-derivation store hello))
(define inherited (package (inherit hello)))
(define inherited-original (package-derivation store inherited))
(define original-bytes (call-with-input-file (derivation-file-name original)
                         get-bytevector-all #:binary #t))

(test-group "CA-4 package policy"
  (test-assert "an inherited package is input-addressed until selected"
    (and (not (addressing original))
         (not (addressing inherited-original))
         (string=? (derivation-file-name original)
                   (derivation-file-name inherited-original))))

  (test-assert "the selected policy does not change the upstream derivation"
    (let ((again (parameterize ((%content-addressed-packages (list inherited)))
                   (package-derivation store hello))))
      (and (string=? (derivation-file-name original)
                     (derivation-file-name again))
           (equal? original-bytes
                   (call-with-input-file (derivation-file-name again)
                     get-bytevector-all #:binary #t))
           (equal? original-bytes
                   (call-with-input-file (derivation-file-name inherited-original)
                     get-bytevector-all #:binary #t)))))

  (test-assert "an explicitly selected inherited hello is content-addressed"
    (let ((selected
           (parameterize ((%content-addressed-packages (list inherited)))
             (package-derivation store inherited))))
      (and (string=? (addressing selected) "content")
           (not (string=? (derivation-file-name inherited-original)
                          (derivation-file-name selected)))
           (equal? (recipe-fields inherited-original) (recipe-fields selected))
           (placeholder-path? (derivation->output-path selected))
           (not (placeholder-path? (derivation->output-path original))))))

  (test-assert "an explicitly opaque package uses resolved addressing"
    (let* ((opaque (package (inherit hello)
                     (properties '((self-references . opaque)))))
           (selected (package-derivation store opaque)))
      (and (string=? (addressing selected) "resolved")
           (equal? (recipe-fields original) (recipe-fields selected))
           (placeholder-path? (derivation->output-path selected))))))

(test-group "CA-4 builder inside the derivation"
  (let* ((guile (package-derivation store %bootstrap-guile))
         (embedded
          (run-with-store store
            (gexp->derivation "ca-embedded-builder"
                              #~(call-with-output-file (getenv "out")
                                  (lambda (p) (display (getenv "out") p)))
                              #:guile-for-build guile
                              #:addressing 'content))))
    (test-assert "a gexp without explicit output references declares out"
      (and (equal? '("out") (derivation-output-names embedded))
           (null? (derivation-sources embedded))
           (string-prefix? "$NIX_BUILD_TOP/"
                           (last (derivation-builder-arguments embedded)))
           (assoc-ref (derivation-builder-environment-vars embedded) "__files")
           (assoc-ref (derivation-builder-environment-vars embedded)
                     "__file:ca-embedded-builder-builder")))
    (test-assert "the embedded builder runs and finalizes its output"
      (begin
        (build-derivations store (list embedded))
        (let ((out (realised-output-path store embedded "out")))
          (and (valid-path? store out)
               (string=? out (call-with-input-file out get-string-all))))))))

(close-connection store)
(test-end)
