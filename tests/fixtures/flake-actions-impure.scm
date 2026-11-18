(flake (format 1)
 (outputs (lambda (in)
  (use-modules (guix tests) (guix build-system trivial)
               (gnu packages bootstrap))
  ;; The version depends on an ambient environment variable.  The confined
  ;; (pure) evaluator has no such variable, so it lowers version "pure"; the
  ;; acting process sees F6_GATE_PROBE=impure and lowers a different
  ;; derivation.  The reconfigure gate must refuse.
  (define pkg (dummy-package "f6-impure"
                (version (or (getenv "F6_GATE_PROBE") "pure"))
                (build-system trivial-build-system)
                (arguments (quasiquote (#:guile ,%bootstrap-guile
                                        #:builder (mkdir %output))))))
  `((packages (sample . ,pkg))))))