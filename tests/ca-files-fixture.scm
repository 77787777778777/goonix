;;; goonix: CA-7 file-level substitution fixture.  Builds V1 (three files) and
;;; V2 (one file changed) in the server store with shell-script derivations
;;; (an interned shell, no bootstrap Guile), writes their names to a file.
(use-modules (guix) (guix tests) (guix store) (guix derivations)
             (ice-9 match) (srfi srfi-1))

(define store (open-connection))
(%graft? #f)
(define shell-source (getenv "GOONIX_SHELL_SOURCE"))
(define shell (add-to-store store "ca-files-shell" #t "sha256" shell-source))
(define mkdir-source (getenv "GOONIX_MKDIR_SOURCE"))
(define mkdir-path (add-to-store store "ca-files-mkdir" #t "sha256" mkdir-source))

(define (item name a b c)
  (let ((script (format #f "'~a' \"$out\"; printf '%s' '~a' > \"$out/a\"; printf '%s' '~a' > \"$out/b\"; printf '%s' '~a' > \"$out/c\""
                        mkdir-path a b c)))
    (derivation store name shell `("-c" ,script)
                #:sources (list shell mkdir-path)
                #:addressing 'input)))

(define v1 (item "ca-files-v1" "alpha" "beta" "gamma"))
(build-derivations store (list v1))
;; V2 changes only file "b".
(define v2 (item "ca-files-v2" "alpha" "BETA-CHANGED" "gamma"))
(build-derivations store (list v2))

(define out (match (command-line) ((_ file) file)))
(call-with-output-file out
  (lambda (port)
    (write (list (list 'v1-name (basename (derivation->output-path v1)))
                 (list 'v2-name (basename (derivation->output-path v2)))
                 (list 'v1-output (derivation->output-path v1))
                 (list 'v2-output (derivation->output-path v2)))
           port)
    (newline port)))
(close-connection store)