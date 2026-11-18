(flake (format 1)
 (outputs (lambda (in)
  (use-modules (guix tests) (guix build-system trivial)
               (gnu packages bootstrap))
  (define pkg (dummy-package "f6-shell-pkg"
                (build-system trivial-build-system)
                (arguments (quasiquote (#:guile ,%bootstrap-guile
                                        #:builder (mkdir %output))))))
  `((packages (sample . ,pkg))
    (shells (default . ,pkg)
            (named . ,pkg))))))