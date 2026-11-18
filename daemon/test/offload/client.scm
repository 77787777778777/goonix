(use-modules (guix store) (guix tests) (guix derivations) (guix packages)
             (ice-9 match) (ice-9 textual-ports) (srfi srfi-34))

(define (assert value message)
  (unless value (error message)))

(match (command-line)
  ((program name expected-status)
   (let* ((root (getenv "HS5_ROOT"))
          (expected-status (string->number expected-status))
          (s (open-connection (getenv "GUIX_DAEMON_SOCKET"))))
     (set-build-options s #:use-substitutes? #f #:print-build-trace #t
                        #:offload? (not (string=? name "no-offload")))
     (let* ((script (add-text-to-store s (string-append "builder-" name ".sh")
                      "printf local > $out\nprintf local >> \"$HS5_ROOT/local-builds\"\n"
                      (list)))
            (bash (add-to-store s "bash" #t "sha256"
                     (search-bootstrap-binary "bash" (%current-system))))
            (drv (derivation s (string-append "offload-" name) bash
                   (list "-e" script)
                   #:env-vars `(("HS5_ROOT" . ,root))
                   #:inputs (list (list bash) (list script))))
            (out (derivation->output-path drv)))
       (guard (c ((store-protocol-error? c)
                  (assert (> expected-status 0)
                    (format #f "unexpected failure: ~s" (store-protocol-error-message c)))
                  (assert (= expected-status (store-protocol-error-status c))
                    (format #f "wrong status: expected ~s got ~s: ~s"
                            expected-status (store-protocol-error-status c)
                            (store-protocol-error-message c)))
                  (assert (not (valid-path? s out)) "failed helper produced valid output")
                  (format #t "~a status=~s no-local-output=~s\n" name
                          (store-protocol-error-status c) (not (file-exists? out)))))
         (build-derivations s (list drv))
         (assert (= expected-status 0) "unexpected successful build")
         (assert (valid-path? s out) "accepted output is not registered")
         (assert (string=? (call-with-input-file out get-string-all)
                   (if (member name '("accept" "postpone")) "produced by helper\n" "local"))
           "wrong output contents")
         (format #t "~a success output=~s\n" name out))
       (close-connection s)))))
