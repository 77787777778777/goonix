;;; goonix: tests for acting flake commands without host changes.
(define-module (test-flake-actions)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix tests)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix build utils)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (guix scripts flake)
  #:use-module ((guix flakes evaluate) #:select (evaluate-flake-shell))
  #:use-module (guix flakes support)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-1))

(test-begin "flake-actions")
(define store (open-connection-for-tests))

(define (write-file* path contents)
  (mkdir-p (dirname path))
  (call-with-output-file path (lambda (port) (display contents port))))

(define (init-repo directory)
  (system* "git" "-C" directory "init" "-q")
  (system* "git" "-C" directory "-c" "user.name=goonix-test"
           "-c" "user.email=goonix-test@example.invalid" "commit" "--allow-empty" "-qm" "initial"))

(define (commit-all directory)
  (system* "git" "-C" directory "add" "-A")
  (system* "git" "-C" directory "-c" "user.name=goonix-test"
           "-c" "user.email=goonix-test@example.invalid" "commit" "-qm" "fixture"))

(define (with-action-fixture fixture proc)
  (call-with-temporary-directory
   (lambda (directory)
     (init-repo directory)
     (copy-file (string-append "tests/fixtures/" fixture)
                (string-append directory "/flake.scm"))
     (commit-all directory)
     (proc directory))))

;; What 'guix shell' enters is evaluated in the container: only the profile
;; derivation and the variables come back.  These tests build the profiles
;; and look inside, so a shell that ignored its name or its packages fails.
(define (shell-profile directory name . rest)
  "Return the built profile of the shell NAME and its variables."
  (let-values (((drv environment)
                (apply evaluate-flake-shell store directory name
                       #:bootstrap? #t rest)))
    (build-derivations store (list drv))
    (values (derivation->output-path drv) environment)))

(define (in-profile? profile tool)
  (file-exists? (string-append profile "/bin/" tool)))

(test-assert "a shell is the profile of its packages, with its variables"
  (with-action-fixture
   "flake-shells.scm"
   (lambda (directory)
     (let-values (((profile environment) (shell-profile directory 'default)))
       (and (equal? '(("FLAKE_SHELL" . "default")) environment)
            (in-profile? profile "flake-tool-one")
            (not (in-profile? profile "flake-tool-two")))))))

(test-assert "another name selects another shell"
  (with-action-fixture
   "flake-shells.scm"
   (lambda (directory)
     (let-values (((profile environment) (shell-profile directory 'other)))
       (and (null? environment)
            (in-profile? profile "flake-tool-two")
            (not (in-profile? profile "flake-tool-one")))))))

(test-assert "a package that is no shell is entered as a shell of itself"
  (with-action-fixture
   "flake-shells.scm"
   (lambda (directory)
     (let-values (((profile environment) (shell-profile directory 'tool)))
       (in-profile? profile "flake-tool-two")))))

(test-assert "the development environment of a package leaves the package out"
  (with-action-fixture
   "flake-shells.scm"
   (lambda (directory)
     (let-values (((profile environment)
                   (shell-profile directory 'tool #:development? #t)))
       (not (in-profile? profile "flake-tool-two"))))))

(test-assert "a missing shell is an error unless it is optional"
  (with-action-fixture
   "flake-shells.scm"
   (lambda (directory)
     (and (not (evaluate-flake-shell store directory 'missing
                                     #:optional? #t #:bootstrap? #t))
          (guard (condition (#t #t))
            (evaluate-flake-shell store directory 'missing #:bootstrap? #t)
            #f)))))

(test-assert "flake checks lower a trivial check"
  (with-action-fixture
   "flake-actions-checks.scm"
   (lambda (directory)
     (let ((drv (flake-output-derivation store directory 'checks 'ok '())))
       (string-suffix? "f6-check-0.drv" (derivation-file-name drv))))))

(test-assert "pure and acting derivations agree for a pure package"
  (with-action-fixture
   "flake-actions-gate.scm"
   (lambda (directory)
     (let* ((object (flake-checked-output store directory 'packages 'sample '()))
            (drv (lower-flake-object store object "sample" "x86_64-linux" #f)))
       (string-suffix? "f6-gate-0.drv" (derivation-file-name drv))))))

(test-assert "acting gate refuses an environment-dependent derivation"
  (with-action-fixture
   "flake-actions-impure.scm"
   (lambda (directory)
     (let ((old (getenv "F6_GATE_PROBE")))
       (dynamic-wind
         (lambda () (setenv "F6_GATE_PROBE" "impure"))
         (lambda ()
           (catch 'quit
             (lambda ()
               (flake-checked-output store directory 'packages 'sample '())
               #f)
             (lambda _ #t)))
         (lambda () (if old (setenv "F6_GATE_PROBE" old)
                        (unsetenv "F6_GATE_PROBE"))))))))

(test-assert "app output lowers to an executable program-file derivation"
  (with-action-fixture
   "flake-actions-shell-app.scm"
   (lambda (directory)
     (let ((drv (flake-output-derivation store directory 'apps 'hi '())))
       (string-suffix? "f6-app-0.drv" (derivation-file-name drv))))))

(test-end)
