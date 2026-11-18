(flake (format 1)
 (outputs (lambda (in)
  (use-modules (guix tests) (guix build-system trivial)
               (gnu packages bootstrap))
  `((packages (sample . ,(dummy-package "sample"
                          (build-system trivial-build-system)
                          (arguments (quasiquote
                                      (#:guile ,%bootstrap-guile
                                       #:builder (mkdir %output)))))))))))
