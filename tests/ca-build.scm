;;; goonix: end-to-end tests of content-addressed derivations.
(use-modules (guix)
             (guix tests)
             (guix ca)
             (gnu packages bootstrap)
             (guix derivations)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-34)
             (srfi srfi-64)
             (ice-9 textual-ports))

(test-begin "ca-build")
(define store (open-connection-for-tests))
(%graft? #f)
(define bash (add-to-store store "bash" #t "sha256"
                           (search-bootstrap-binary "bash" (%current-system))))
(define mkdir (add-to-store store "mkdir" #t "sha256"
                            (search-bootstrap-binary "mkdir" (%current-system))))
(define guile-drv (package-derivation store %bootstrap-guile))
(build-derivations store (list guile-drv))
(define bootstrap-guile (string-append (derivation->output-path guile-drv)
                                      "/bin/guile"))
(define bootstrap-guile-modules
  (string-append (derivation->output-path guile-drv)
                 "/share/guile/2.0"))
(define* (shell-drv name script #:key (inputs '()) (sources '())
                    (addressing 'content) (env-vars '()) (outputs '("out")))
  ;; Keep one builder text per script. A different source path for an
  ;; otherwise identical input-addressed builder changes its derivation.
  (let* ((input-paths (map (lambda (input)
                             (derivation->output-path (car input)))
                           inputs))
         (builder (string-append
                   "(use-modules (ice-9 rdelim))\n"
                   "(define (read-all port)\n"
                   " (let loop ((lines '()))\n"
                   "  (let ((line (read-line port 'concat)))\n"
                   "   (if (eof-object? line)\n"
                   "    (string-concatenate-reverse lines)\n"
                   "    (loop (cons line lines))))))\n"
                   "(define out (getenv \"out\"))\n"
                   "(define inputs (call-with-input-string (getenv \"input-paths\") read))\n"
                   "(define (mkdir-p directory)\n"
                   " (zero? (system* \"" mkdir "\" \"-p\" directory)))\n"
                   script "\n"))
         (builder-file (add-text-to-store store "builder.scm" builder)))
    (derivation store name bootstrap-guile
                `("--no-auto-compile"
                  ,(if (eq? addressing 'input)
                       builder-file
                       "$NIX_BUILD_TOP/builder.scm"))
                #:inputs `(,guile-drv ,@inputs)
                #:outputs outputs
                #:sources (if (eq? addressing 'input)
                              (list builder-file)
                              (list mkdir))
                #:addressing addressing
                #:env-vars
                `(("GUILE_LOAD_PATH" . ,bootstrap-guile-modules)
                  ("script" . ,script)
                  ("input-paths" . ,(object->string input-paths))
                  ("__files" . "builder.scm")
                  ("__file:builder.scm" . ,builder)
                  ,@env-vars))))

(define (write-output text)
  (string-append "(call-with-output-file out (lambda (p) (display " 
                 (object->string text) " p)))"))

(define (read-all file) (call-with-input-file file get-string-all))
(define (realised drv) (realised-output-path store drv "out"))

(test-assert "input-addressed derivation bytes remain unchanged"
  (let ((drv (shell-drv "legacy-identity" (write-output "legacy")
                        #:addressing 'input)))
    (and (not (assoc "goonixAddressing" (derivation-builder-environment-vars drv)))
         (not (placeholder-path? (derivation->output-path drv)))
         (not (token-store-path? (derivation->output-path drv)))
         (string=? (derivation-file-name drv)
                   (derivation-file-name
                    (derivation store "legacy-identity" bootstrap-guile
                                `("--no-auto-compile"
                                  ,(car (derivation-sources drv)))
                                #:inputs (list guile-drv)
                                #:sources (derivation-sources drv)
                                #:addressing 'input
                                #:env-vars (derivation-builder-environment-vars drv)))))))

(test-assert "an unused variable changes neither output nor build"
  (let* ((one (shell-drv "ca-unused" (write-output "stable")
                         #:env-vars '(("unused" . "one"))))
         (two (shell-drv "ca-unused" (write-output "stable")
                         #:env-vars '(("unused" . "two"))))
         (token (derivation->output-path one)))
    (test-assert "unused-variable derivation has a placeholder"
      (placeholder-path? token))
    (test-assert "unused-variable derivation identities differ"
      (not (string=? (derivation-file-name one) (derivation-file-name two))))
    (test-assert "unused-variable derivations build"
      (build-derivations store (list one two)))
    (test-equal "unused-variable derivations share one result"
      (realised one) (realised two))
    (test-equal "unused-variable result contents"
      "stable" (read-all (realised one)))
    (test-assert "unused-variable builds are not cached failures"
      (null? (query-failed-paths store)))))

(test-assert "a second identical CA result is reused without overwriting"
  (let* ((one (shell-drv "ca-existing" (write-output "same")
                         #:env-vars '(("unused" . "one"))))
         (two (shell-drv "ca-existing" (write-output "same")
                         #:env-vars '(("unused" . "two")))))
    (build-derivations store (list one))
    (let ((path (realised one)))
      (and (build-derivations store (list two))
           (string=? path (realised two))
           (string=? "same" (read-all path))))))

(test-assert "whole self references, file names and symlinks round-trip"
  (let* ((drv (shell-drv "ca-relocation"
                (string-append
                 "(mkdir out)\n"
                 "(call-with-output-file (string-append out \"/reference\") (lambda (p) (display out p)))\n"
                 "(mkdir-p (string-append out \"/\" (basename out)))\n"
                 "(symlink (basename out) (string-append out \"/link\"))\n"
                 "(call-with-output-file (string-append out \"/executable\") (lambda (p) (display \"#!/bin/sh\\n\" p)))\n"
                 "(chmod (string-append out \"/executable\") #o755)\n")))
         (path (begin (build-derivations store (list drv)) (realised drv))))
    (and (not (string-contains (call-with-input-file
                                  (string-append path "/reference") get-string-all)
                               (self-token "out")))
         (string=? (basename path)
                   (readlink (string-append path "/link")))
         (file-exists? (string-append path "/" (basename path)))
         (verify-store store #:check-contents? #t #:repair? #f))))

(test-assert "identical dependency content cuts off the downstream chain"
  (let* ((a1 (shell-drv "ca-a" (write-output "A") #:env-vars '(("v" . "1"))))
         (a2 (shell-drv "ca-a" (write-output "A") #:env-vars '(("v" . "2"))))
         (b (shell-drv "ca-b"
                       "(call-with-output-file out (lambda (p) (display (call-with-input-file (car inputs) (lambda (port) (read-all port))) p) (display \"B\" p)))"
                       #:inputs `((,a1))))
         (c (shell-drv "ca-c"
                       "(call-with-output-file out (lambda (p) (display (call-with-input-file (car inputs) (lambda (port) (read-all port))) p) (display \"C\" p)))"
                       #:inputs `((,b)))))
    (build-derivations store (list a1))
    (build-derivations store (list a2))
    (and (string=? (realised a1) (realised a2))
         (build-derivations store (list c))
         (string=? "ABC" (read-all (realised c))))))

(test-assert "an unresolved placeholder in an output is refused"
  (let* ((a (shell-drv "ca-placeholder-input" (write-output "A")))
         (unresolved (placeholder (derivation->bytevector a) "out"
                                  "ca-placeholder-input"))
         (b (shell-drv "ca-placeholder-output"
                       (string-append
                        "(call-with-output-file out (lambda (p) (display \""
                        unresolved "\" p)))"))))
    (guard (condition ((store-protocol-error? condition)
                       (string-contains (store-protocol-error-message condition)
                                        "placeholder")))
      (build-derivations store (list b))
      #f)))

(test-assert "resolved-addressed fallback builds at its final path"
  (let ((drv (shell-drv "ca-resolved" (write-output "resolved")
                        #:addressing 'resolved)))
    (and (build-derivations store (list drv))
         (direct-store-path? (realised drv))
         (not (placeholder-path? (realised drv)))
         (string=? "resolved" (read-all (realised drv))))))

(test-assert "sibling output references are rewritten and registered"
  (let ((drv (shell-drv "ca-multi"
                        "(mkdir-p out)\n(call-with-output-file (string-append out \"/reference\") (lambda (p) (display (getenv \"lib\") p)))\n(call-with-output-file (getenv \"lib\") (lambda (p) (display \"library\" p)))"
                        #:outputs '("lib" "out"))))
    (build-derivations store (list drv))
    (let ((lib (realised-output-path store drv "lib"))
          (out (realised-output-path store drv "out")))
      (and (string=? lib (read-all (string-append out "/reference")))
           (member lib (references store out))))))

(close-connection store)
(test-end)
