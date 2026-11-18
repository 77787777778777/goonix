(flake (format 1)
 (outputs (lambda (in)
  (use-modules (guix tests) (guix build-system trivial)
               (gnu packages bootstrap) (ice-9 textual-ports))
  (let ((text (string-trim-right (call-with-input-file
                                  (string-append (self-file in "version.txt"))
                                  get-string-all))))
    `((packages (sample . ,(dummy-package (string-append "sample-" text)
                           (build-system trivial-build-system)
                           (arguments (quasiquote
                                       (#:guile ,%bootstrap-guile
                                        #:builder (mkdir %output))))))))))))
