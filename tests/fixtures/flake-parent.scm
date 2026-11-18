(flake (format 1)
 (inputs (child (local "CHILD_PATH")))
 (outputs (lambda (in)
  (use-modules (guix tests) (guix build-system trivial)
               (gnu packages bootstrap))
  (let ((child (input-output in 'child 'packages 'sample)))
    `((packages (root . ,(dummy-package "root"
                          (build-system trivial-build-system)
                          (inputs (list child))
                          (arguments
                           (quasiquote
                            (#:guile (unquote %bootstrap-guile)
                             #:builder (begin
                                         (mkdir %output)
                                         (call-with-output-file
                                             (string-append %output "/child")
                                           (lambda (port)
                                             (display (ungexp child) port)))))))))))))))
