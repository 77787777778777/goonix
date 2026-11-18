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
  #:use-module (guix flakes support)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
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

(test-assert "typed shell description preserves packages and environment"
  (with-action-fixture
   "flake-actions-shell.scm"
   (lambda (directory)
     (let ((description (flake-shell-description store directory 'default '())))
       (and (manifest? (car description))
            (equal? '() (cdr description))
            (string=? "f6-shell-pkg"
                      (package-name (manifest-entry-item
                                     (first (manifest-entries (car description)))))))))))

(test-assert "named shell reference selects the named shell"
  (with-action-fixture
   "flake-actions-shell.scm"
   (lambda (directory)
     (let ((description (flake-shell-description store directory 'named '())))
       (string=? "f6-shell-pkg"
                 (package-name (manifest-entry-item
                                (first (manifest-entries (car description))))))))))

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
