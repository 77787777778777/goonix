(flake (format 1)
 (outputs (lambda (in)
  (use-modules (guix gexp) (guix packages) (guix tests)
               (guix build-system trivial) (gnu packages bootstrap))
  (define app (plain-file "f6-app" "#!/bin/sh\necho f6-app-ran\n"))
  `((apps (hi . ,(dummy-package "f6-app"
                  (build-system trivial-build-system)
                  (arguments
                   (list #:guile %bootstrap-guile
                         #:builder
                         (gexp (begin
                                 (mkdir %output)
                                 (mkdir (string-append %output "/bin"))
                                 (copy-file (ungexp app)
                                            (string-append %output "/bin/f6-app"))
                                 (chmod (string-append %output "/bin/f6-app")
                                        #o555))))))))))))
