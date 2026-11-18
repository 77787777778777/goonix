(flake (format 1)
 (outputs (lambda (in)
  (use-modules (guix gexp))
  `((apps (hi . ,(program-file "f6-app"
                    (gexp (begin (display "f6-app-ran") (newline))))))))))