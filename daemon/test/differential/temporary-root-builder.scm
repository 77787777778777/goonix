(use-modules (guix store) (guix derivations) (guix tests)
             (gnu packages bootstrap) (ice-9 textual-ports))

(define fixture (getenv "HS7_FIXTURE"))
(define store (open-connection (getenv "HS7_HOLDER_SOCKET")))
(set-build-options store #:use-substitutes? #f #:offload? #f)
(%graft? #f)
(define shell (add-to-store store "bash" #t "sha256"
               (search-bootstrap-binary "bash" "x86_64-linux")))
(define input (add-text-to-store store "collector-live-input" "input retained by the running builder\n"))
(define script
  (add-text-to-store store "collector-live-builder.sh"
    (string-append
      "set -eu\n"
      "printf '%s\\n' '" input "' > '" fixture "/builder-input'\n"
      "printf '%s\\n' \"$out\" > '" fixture "/builder-output'\n"
      "printf '%s\\n' \"$NIX_BUILD_TOP\" > '" fixture "/builder-work'\n"
      "read -r input_line < '" input "'\n"
      "printf '%s\\n' \"$input_line\" > '" fixture "/input-copy'\n"
      "printf running > '" fixture "/builder-started'\n"
      "while test ! -e '" fixture "/release-builder'; do\n"
      "  read -t 1 _ < '" fixture "/tick' || true\n"
      "done\n"
      "printf done > '" fixture "/builder-finished'\n"
      "printf '%s\\n' \"$input_line\" > \"$out\"\n")))
(define drv (derivation store "collector-live-build" shell
                        (list "-e" script)
                        #:sources (list shell script input)))
(call-with-output-file (string-append fixture "/builder-drv")
  (lambda (port) (display (derivation-file-name drv) port) (newline port)))
(build-derivations store (list drv))
(unless (file-exists? (string-append fixture "/builder-finished"))
  (error "builder was not released"))
(unless (valid-path? store (derivation->output-path drv))
  (error "released builder did not register its output"))
(close-connection store)
(call-with-output-file (string-append fixture "/build-client-finished")
  (lambda (port) (display "done" port) (newline port)))
