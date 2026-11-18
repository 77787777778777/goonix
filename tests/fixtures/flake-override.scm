(flake (format 1)
 (outputs (lambda (in)
  (use-modules (guix tests) (guix build-system trivial)
               (gnu packages bootstrap) (guix flakes support))
  (let* ((original (dummy-package "original"
                    (build-system trivial-build-system)
                    (arguments (quasiquote
                                (#:guile ,%bootstrap-guile
                                 #:builder (mkdir %output))))))
         (wrapper (dummy-package "wrapper"
                    (build-system trivial-build-system)
                    (inputs (list original))
                    (arguments (quasiquote
                                (#:guile ,%bootstrap-guile
                                 #:builder (begin (mkdir %output)
                                                 (symlink (ungexp original)
                                                          (string-append %output "/original"))))))))
         (consumer (dummy-package "consumer"
                     (build-system trivial-build-system)
                     (inputs (list original))
                     (arguments (quasiquote
                                 (#:guile ,%bootstrap-guile
                                  #:builder (begin (mkdir %output)
                                                  (symlink (ungexp original)
                                                           (string-append %output "/original")))))))))
    `((packages (consumer . ,consumer) (wrapper . ,wrapper))
      (overrides (original . ,(override (original original)
                                       (replacement wrapper)))))))))
