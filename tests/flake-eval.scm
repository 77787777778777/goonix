;;; goonix: contained flake evaluation.
(define-module (test-flake-eval)
  #:use-module (guix flakes)
  #:use-module (guix flakes evaluate)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix tests)
  #:use-module (guix build utils)
  #:use-module (ice-9 textual-ports)
  #:use-module ((guix utils) #:select (call-with-temporary-directory string-replace-substring))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64))

(test-begin "flake-eval")
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

(test-assert "a package output is lowered with declared options"
  (call-with-temporary-directory
   (lambda (directory)
     (system* "git" "-C" directory "init" "-q")
     (call-with-output-file (string-append directory "/flake.scm")
       (lambda (port)
         (display "(flake (format 1)\n\
 (options (variant string \"one\" \"Variant\"))\n\
 (outputs (lambda (in)\n\
  (use-modules (guix tests) (guix build-system trivial)\n\
               (gnu packages bootstrap))\n\
  `((packages (sample . ,(dummy-package \"sample\" (version (option in 'variant))\n\
   (build-system trivial-build-system)\n\
   (arguments (quasiquote (#:guile ,%bootstrap-guile\n\
                           #:builder (mkdir %output)))))))))))"
                  port)))
     (system* "git" "-C" directory "add" "flake.scm")
     (let* ((one (evaluate-flake store directory 'packages 'sample))
            (two (evaluate-flake store directory 'packages 'sample
                                 #:options '((variant . "two")))))
       (test-assert "default option determines the output name"
         (string-suffix? "sample-one.drv" (derivation-file-name one)))
       (test-assert "explicit option determines the output name"
         (string-suffix? "sample-two.drv" (derivation-file-name two)))
       (test-equal "flake show lists the default derivation"
         `((packages sample ,(derivation-file-name one)))
         (evaluate-flake-outputs store directory))
       (test-assert "option changes the derivation"
         (not (string=? (derivation-file-name one)
                        (derivation-file-name two))))))))

(test-assert "a dependency output crosses as a derivation handle"
  (call-with-temporary-directory
   (lambda (root)
     (let ((child (string-append root "/child"))
           (parent (string-append root "/parent")))
       (mkdir-p child) (mkdir-p parent) (init-repo child) (init-repo parent)
       (write-file* (string-append child "/version.txt") "original\n")
       (copy-file "tests/fixtures/flake-pinned-child.scm"
                  (string-append child "/flake.scm"))
       (commit-all child)
       (copy-file "tests/fixtures/flake-parent.scm"
                  (string-append parent "/flake.scm"))
       (let ((port (open-input-file (string-append parent "/flake.scm"))))
         (let ((text (get-string-all port)))
           (close-port port)
           (write-file* (string-append parent "/flake.scm")
                        (string-replace-substring text "CHILD_PATH" child))))
       (commit-all parent)
       (lock-flake parent)
       (let ((root-drv (evaluate-flake store parent 'packages 'root)))
         (write-file* (string-append child "/version.txt") "later change\n")
         (let ((locked-root-drv (evaluate-flake store parent 'packages 'root)))
           (and (string-suffix? "root-0.drv" (derivation-file-name root-drv))
                (string=? (derivation-file-name root-drv)
                          (derivation-file-name locked-root-drv))
                (= 1 (length (filter (lambda (input)
                                       (string-suffix? "sample-original-0.drv"
                                                       (derivation-input-path input)))
                                     (derivation-inputs root-drv)))))))))))

(test-assert "source-set imports exactly the selected tracked files"
  (call-with-temporary-directory
   (lambda (directory)
     (init-repo directory)
     (write-file* (string-append directory "/one.txt") "one")
     (write-file* (string-append directory "/two.txt") "two")
     (copy-file "tests/fixtures/flake-source-set.scm"
                (string-append directory "/flake.scm"))
     (commit-all directory)
     (let ((first (evaluate-flake store directory 'packages 'sample)))
       (write-file* (string-append directory "/two.txt") "changed")
       (let ((second (evaluate-flake store directory 'packages 'sample)))
         (write-file* (string-append directory "/one.txt") "changed")
         (let ((third (evaluate-flake store directory 'packages 'sample)))
           (and (string=? (derivation-file-name first) (derivation-file-name second))
                (not (string=? (derivation-file-name first) (derivation-file-name third))))))))))

;; The evaluator asks the host for files by name.  A name that passes through
;; a tracked symbolic link would make the host copy a file from outside the
;; tree into the store.
(test-assert "source-set does not follow a symbolic link out of the tree"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((project (string-append directory "/project"))
           (outside (string-append directory "/outside")))
       (mkdir-p project)
       (write-file* (string-append outside "/secret") "secret")
       (init-repo project)
       (symlink outside (string-append project "/link"))
       (write-file* (string-append project "/flake.scm")
                    (string-replace-substring
                     (call-with-input-file "tests/fixtures/flake-source-set.scm"
                       get-string-all)
                     "\"one.txt\"" "\"link/secret\""))
       (commit-all project)
       (guard (condition
               ((message-condition? condition)
                (string-contains (condition-message condition)
                                 "symbolic link")))
         (evaluate-flake store project 'packages 'sample)
         #f)))))

(test-assert "a persistent cache hit starts no evaluator"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((cache (string-append directory "/cache"))
           (project (string-append directory "/project")))
       (mkdir-p project)
       (let ((old-cache (getenv "XDG_CACHE_HOME")))
         (dynamic-wind
           (lambda () (setenv "XDG_CACHE_HOME" cache))
           (lambda ()
             (init-repo project)
             (copy-file "tests/fixtures/flake-child.scm"
                        (string-append project "/flake.scm"))
             (commit-all project)
             (let ((first (evaluate-flake store project 'packages 'sample)))
               (hash-clear! %evaluation-cache)
               (let ((starts (evaluator-starts)))
                 (let ((second (evaluate-flake store project 'packages 'sample)))
                   (and (= starts (evaluator-starts))
                        (string=? (derivation-file-name first)
                                  (derivation-file-name second)))))))
           (lambda ()
             (if old-cache (setenv "XDG_CACHE_HOME" old-cache)
                 (unsetenv "XDG_CACHE_HOME")))))))))

(test-assert "typed shells lower to profile derivations"
  (call-with-temporary-directory
   (lambda (directory)
     (init-repo directory)
     (copy-file "tests/fixtures/flake-shell.scm"
                (string-append directory "/flake.scm"))
     (commit-all directory)
     (let ((drv (evaluate-flake store directory 'shells 'default)))
       (string-suffix? "profile.drv" (derivation-file-name drv))))))

(test-assert "overrides replace inputs at lowering time and exempt wrappers"
  (call-with-temporary-directory
   (lambda (directory)
     (init-repo directory)
     (copy-file "tests/fixtures/flake-override.scm"
                (string-append directory "/flake.scm"))
     (commit-all directory)
     (let ((consumer (evaluate-flake store directory 'packages 'consumer))
           (wrapper (evaluate-flake store directory 'packages 'wrapper)))
       (and (any (lambda (input)
                   (string-suffix? "wrapper-0.drv"
                                   (derivation-input-path input)))
                 (derivation-inputs consumer))
            (any (lambda (input)
                   (string-suffix? "original-0.drv"
                                   (derivation-input-path input)))
                 (derivation-inputs wrapper)))))))

(test-end)
