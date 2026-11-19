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
(define inherited-hello (package (inherit hello)))
(define hello-bytes (bytes upstream-hello))
(define upstream-closure (upstream-package-closure (list hello)))

(test-group "mixed policy preserves upstream and propagates through inputs"
  (let ((mixed-hello
         (parameterize ((%package-addressing-policy 'mixed)
                        (%upstream-packages upstream-closure))
           (package-derivation store hello)))
        (mixed-inherited
         (parameterize ((%package-addressing-policy 'mixed)
                        (%upstream-packages upstream-closure))
           (package-derivation store inherited-hello))))
    (test-equal "mixed upstream hello is byte-identical"
      hello-bytes (bytes mixed-hello))
    (test-assert "an inherited hello is content-addressed without selection"
      (string=? "content" (addressing mixed-inherited)))
    (test-assert "an inherited hello differs from upstream hello"
      (not (string=? (derivation-file-name upstream-hello)
                     (derivation-file-name mixed-inherited))))
    (let ((dependent
           (package
             (inherit inherited-hello)
             (name "ca-policy-unselected-dependent")
             (inputs (list (list "hello" inherited-hello))))))
      (test-assert "an unselected dependent of inherited hello is content-addressed"
        (string=? "content"
                  (parameterize ((%package-addressing-policy 'mixed)
                                 (%upstream-packages upstream-closure))
                    (addressing (package-derivation store dependent))))))))

(test-group "the addressing cache includes the current mode on every call"
  (let ((input (parameterize ((%package-addressing-policy 'input)
                              (%upstream-packages (upstream-package-set (set))))
                 (package-derivation store inherited-hello)))
        (content (parameterize ((%package-addressing-policy 'content)
                                (%upstream-packages (upstream-package-set (set))))
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
  (let ((drv (package-derivation store package)))
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

(test-group "real builds use the realised input path"
  (parameterize ((%package-addressing-policy 'mixed)
                 (%upstream-packages (upstream-package-set (list->setq (list %bootstrap-guile)))))
    (let* ((a-result (build-and-read a))
           (b-result (build-and-read b)))
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
