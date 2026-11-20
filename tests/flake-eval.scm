;;; goonix: tests for the evaluation of flakes.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (test-flake-eval)
  #:use-module (guix flakes lock)
  #:use-module (guix flakes resolve)
  #:use-module (guix flakes syntax)
  #:use-module (guix flakes evaluate)
  #:use-module ((guix pure-eval) #:select (pure-eval-supported? call-with-evaluator))
  #:use-module ((guix git) #:select (%repository-cache-directory))
  #:use-module (guix store)
  #:use-module ((guix config) #:select (%guile))
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module ((guix secrets age) #:select (generate-x25519-identity
                                             x25519-identity-public
                                             encrypt-age
                                             decrypt-age-file))
  #:use-module (rnrs bytevectors)
  #:use-module (guix tests)
  #:use-module (guix tests git)
  #:use-module ((guix channels)
                #:select (channel-name channel-url channel-commit))
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module ((guix build utils)
                #:select (mkdir-p with-directory-excursion
                          delete-file-recursively))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(test-begin "flake-eval")

(define %store (open-connection-for-tests))

;; The evaluator runs in a container.
(unless (pure-eval-supported?)
  (test-skip 100))

(define (contents file)
  (call-with-input-file file get-string-all))

(define (write-file file text)
  (mkdir-p (dirname file))
  (call-with-output-file file
    (lambda (port) (display text port))))

(define (git directory . arguments)
  (let* ((pipe (apply open-pipe* OPEN_READ (git-command) "-C" directory
                      arguments))
         (output (string-trim-right (get-string-all pipe))))
    (unless (zero? (close-pipe pipe))
      (error "git failed" arguments))
    output))

(define (commit! directory . files)
  "Add FILES, pairs of a name and a text, to the repository in DIRECTORY,
which is made if need be; commit, and return the commit."
  (with-git-repository directory
      `(,@(map (match-lambda
                 ((name . text) `(add ,name ,text)))
               files)
        (commit "a commit"))
    (git directory "rev-parse" "HEAD")))

(define %counter 0)

(define (call-with-workspace proc)
  "Call PROC with a new directory.  It is below the current directory, since
the container has a /tmp of its own; checkouts and evaluations are cached
below it."
  (set! %counter (+ 1 %counter))
  (let ((directory (string-append (getcwd) "/t-flake-eval-"
                                  (number->string (getpid)) "-"
                                  (number->string %counter)))
        (cache     (getenv "XDG_CACHE_HOME")))
    (dynamic-wind
      (lambda ()
        (mkdir-p directory)
        (setenv "XDG_CACHE_HOME" (string-append directory "/cache")))
      (lambda ()
        (parameterize ((%repository-cache-directory
                        (string-append directory "/checkouts")))
          (hash-clear! %evaluations)
          (proc directory)))
      (lambda ()
        (if cache
            (setenv "XDG_CACHE_HOME" cache)
            (unsetenv "XDG_CACHE_HOME"))
        (delete-file-recursively directory)))))

(define-syntax-rule (with-workspace directory body ...)
  (call-with-workspace (lambda (directory) body ...)))

(define-syntax-rule (raises? body ...)
  (guard (condition (#t #t))
    body ...
    #f))

(define %modules
  ;; What the test flakes use.
  "(use-modules (guix) (guix build-system trivial) (gnu packages bootstrap))")

(define %tool
  ;; A package that builds with the bootstrap Guile alone.  Its builder
  ;; writes TEXT, a gexp, to its output.
  "(define* (tool name text #:key (inputs '()) (source #f))
     (package
       (name name) (version \"0\") (source source)
       (build-system trivial-build-system)
       (arguments
        (list #:guile %bootstrap-guile
              #:builder
              #~(call-with-output-file #$output
                  (lambda (port) (display #$text port)))))
       (inputs inputs)
       (home-page #f) (synopsis #f) (description #f) (license #f)))")

(define (flake-text fields . body)
  "Return the text of a flake.scm with FIELDS, a list of S-expressions such
as (inputs ...), whose 'outputs' procedure has the texts of BODY as its
body, after the definitions the tests share."
  (format #f "(flake (format 1)~%~{ ~s~%~} (outputs (lambda (in)~%~a~%~a~%\
~{~a~%~})))~%"
          fields %modules %tool body))

(define (flake! directory fields . body)
  "Make DIRECTORY a checkout that tracks a flake.scm, and return it."
  (commit! directory `("flake.scm" . ,(apply flake-text fields body)))
  directory)

(define (write-vault-declaration directory declarations)
  "Write DECLARATIONS as data-only vault metadata for evaluation tests."
  (write-file (string-append directory "/secrets/vault.scm")
              (format #f "(vault (format 1) (recipients (owner \"~a\")) (secrets ~a))"
                      (x25519-identity-public (generate-x25519-identity))
                      (string-join declarations " "))))

(define (secret-vault! directory . declarations)
  (write-vault-declaration directory declarations)
  (git directory "add" "secrets/vault.scm"))

(define (output-name derivation)
  (derivation->output-path derivation))

(define (built derivation)
  "Build DERIVATION and return the text of its output."
  (build-derivations %store (list derivation))
  (contents (output-name derivation)))


(test-assert "secret references give the same derivation with any identity"
  (with-workspace directory
    (let* ((recipient (generate-x25519-identity))
           (other (generate-x25519-identity))
           (recipient-file (string-append directory "/recipient"))
           (other-file (string-append directory "/other")))
      (write-file recipient-file (string-append recipient "\n"))
      (write-file other-file (string-append other "\n"))
      (commit! directory
               `("flake.scm" . ,(flake-text '()
                   "`((packages (sample . ,(tool \"sample\" (secret in 'token)))))"))
               `("secrets/vault.scm" . ,(format #f
                   "(vault (format 1) (recipients (owner \"~a\")) (secrets (token (kind password) (for owner))))"
                   (x25519-identity-public recipient)))
               `("secrets/token.age" . ,(encrypt-age (string->utf8 "not used by evaluation")
                                                     (list (x25519-identity-public recipient)))))
      (let ((evaluate (lambda (identity)
                        (setenv "GUIX_SECRETS_IDENTITY" identity)
                        (derivation-file-name
                         (evaluate-flake %store directory 'packages 'sample)))))
        (let ((first (evaluate recipient-file))
              (second (evaluate other-file))
              (third (begin (unsetenv "GUIX_SECRETS_IDENTITY")
                            (evaluate #f))))
          (and (string=? first second) (string=? first third)))))))

(test-assert "a public half is available as a tracked file"
  (with-workspace directory
    (flake! directory '()
            "`((packages (sample . ,(tool \"sample\" (secret-public in 'guest)))))")
    (secret-vault! directory "(guest (kind ssh-key) (for owner))")
    (write-file (string-append directory "/secrets/guest.pub")
                "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIpublic\n")
    (git directory "add" "secrets/guest.pub")
    (git directory "-c" "user.name=Goonix" "-c" "user.email=goonix@example.org"
         "commit" "-m" "public half")
    (let ((derivation (evaluate-flake %store directory 'packages 'sample)))
      (build-derivations %store (list derivation))
      (and (string-contains (contents (derivation->output-path derivation))
                            "/secrets/guest.pub")
           (not (string-contains (contents (derivation->output-path derivation))
                                 "PRIVATE KEY"))))))

(test-equal "local options feed flake evaluation without changing the repository"
  "machine-value"
  (with-workspace directory
    (flake! directory '((options (machine string "default" "Machine value")))
            "`((packages (sample . ,(tool \"sample\" (option in 'machine)))))")
    (write-file (string-append directory "/flake.local.scm")
                "(flake-local (version 1) (options (machine \"machine-value\")))")
    (built (evaluate-flake %store directory 'packages 'sample))))

(test-assert "local input overrides still parse alongside options"
  (with-workspace directory
    (let ((input (string-append directory "/input")))
      (mkdir input)
      (write-file (string-append directory "/flake.local.scm")
                  "(flake-local (version 1) (inputs (dep (local \"input\"))) (options (machine \"x\")))")
      (and (string=? "input" (flake-input-path (assq-ref (read-local-overrides directory) 'dep)))
           (string=? "x" (assq-ref (read-local-options directory) 'machine))))))

(test-assert "duplicate local options are refused"
  (with-workspace directory
    (write-file (string-append directory "/flake.local.scm")
                "(flake-local (version 1) (options (machine \"x\")) (options (machine \"y\")))")
    (raises? (read-local-options directory))))

(test-assert "secret names cannot escape their runtime directory"
  (with-workspace directory
    (flake! directory '()
            "`((packages (sample . ,(tool \"sample\" (secret in '..)))))")
    (secret-vault! directory "(token (kind file) (for owner))")
    (write-file (string-append directory "/fixture-name") "dotdot")
    (git directory "add" "fixture-name")
    (raises? (evaluate-flake %store directory 'packages 'sample #:cache? #f))))

(test-assert "secret names cannot contain a slash"
  (with-workspace directory
    (flake! directory '()
            "`((packages (sample . ,(tool \"sample\" (secret in 'a/b)))))")
    (secret-vault! directory "(token (kind file) (for owner))")
    (write-file (string-append directory "/fixture-name") "slash")
    (git directory "add" "fixture-name")
    (raises? (evaluate-flake %store directory 'packages 'sample #:cache? #f))))

(test-assert "secret names cannot end in a dot"
  (with-workspace directory
    (flake! directory '()
            "`((packages (sample . ,(tool \"sample\" (secret in 'token.)))))")
    (secret-vault! directory "(token (kind file) (for owner))")
    (write-file (string-append directory "/fixture-name") "trailing-dot")
    (git directory "add" "fixture-name")
    (raises? (evaluate-flake %store directory 'packages 'sample #:cache? #f))))

(test-assert "secret references require a declaration"
  (with-workspace directory
    (flake! directory '()
            "`((packages (sample . ,(tool \"sample\" (secret in 'missing)))))")
    (write-file (string-append directory "/fixture-name") "missing-declaration")
    (git directory "add" "fixture-name")
    (raises? (evaluate-flake %store directory 'packages 'sample #:cache? #f))))

(test-assert "secret-public requires an ssh-key declaration"
  (with-workspace directory
    (flake! directory '()
            "`((packages (sample . ,(tool \"sample\" (secret-public in 'guest)))))")
    (secret-vault! directory "(guest (kind file) (for owner))")
    (write-file (string-append directory "/secrets/guest.pub") "ssh-ed25519 public\n")
    (raises? (evaluate-flake %store directory 'packages 'sample))))

(test-assert "secret-public refuses a private identity in the public path"
  (with-workspace directory
    (flake! directory '()
            "`((packages (sample . ,(tool \"sample\" (secret-public in 'guest)))))")
    (secret-vault! directory "(guest (kind ssh-key) (for owner))")
    (write-file (string-append directory "/secrets/guest.pub")
                "AGE-SECRET-KEY-1not-a-real-key\n")
    (git directory "add" "secrets/guest.pub")
    (git directory "-c" "user.name=Goonix" "-c" "user.email=goonix@example.org"
         "commit" "-m" "private half in public path")
    (raises? (evaluate-flake %store directory 'packages 'sample))))

(test-assert "the evaluator cannot read identities, decrypt, or read outside files"
  (with-workspace directory
    (let* ((identity (generate-x25519-identity))
           (identity-file (string-append directory "/identity"))
           (vault (string-append directory "/vault"))
           (outside (string-append directory "/outside"))
           (script (string-append directory "/probe.scm"))
           (cipher (encrypt-age (string->utf8 "outside secret") (list (x25519-identity-public identity)))))
      (write-file identity-file (string-append identity "\n"))
      (write-file (string-append vault "/vault.scm")
                  (format #f "(vault (format 1) (recipients (owner \"~a\")) (secrets (token (kind password) (for owner))))"
                          (x25519-identity-public identity)))
      (write-file (string-append vault "/token.age") cipher)
      (write-file outside "outside secret")
      (write-file script
                  (format #f "(use-modules (guix secrets vault) (guix secrets age) (ice-9 rdelim) (ice-9 textual-ports))
(define (try name thunk)
  (catch #t
    (lambda () (thunk) (error \"access was allowed\" name))
    (lambda args
      (unless (or (and (eq? 'system-error (car args))
                       (= (car (list-ref args 4)) 2))
                  (and (equal? \"read-identity\" name)
                       (eq? 'misc-error (car args))))
        (error \"unexpected evaluator error\" name args)))))
(try \"identity\" (lambda () (call-with-input-file ~s get-line)))
(try \"read-identity\" (lambda () (read-identity ~s)))
(try \"decrypt\" (lambda ()
  (let ((v (call-with-input-file ~s read-vault)))
    (show-secret ~s v 'token (read-identity ~s)))))
(try \"outside\" (lambda () (call-with-input-file ~s get-line)))"
                          identity-file identity-file
                          (string-append vault "/vault.scm") vault identity-file outside))
      (and (file-exists? identity-file)
           (file-exists? (string-append vault "/token.age"))
           (file-exists? outside)
           (string=? "outside secret"
                     (utf8->string (decrypt-age-file cipher (list identity))))
           (call-with-evaluator
            (list %guile "--no-auto-compile" "-c" (contents script))
            (lambda (in out) (close-port in) (close-port out) #t)
            #:load-path (list (evaluator-sources %store)))))))
(test-equal "an output is the derivation of the same package outside a flake"
  (derivation-file-name
   (package-derivation
    %store
    (eval `(begin ,@(call-with-input-string
                        (string-append "(" %modules %tool
                                       "(tool \"sample\" \"text\"))")
                      read))
          (make-fresh-user-module))))
  (with-workspace directory
    (derivation-file-name
     (evaluate-flake %store
                     (flake! directory '()
                             "`((packages (sample . ,(tool \"sample\" \
\"text\"))))")
                     'packages 'sample))))

(test-equal "an output builds"
  "text"
  (with-workspace directory
    (built (evaluate-flake %store
                           (flake! directory '()
                                   "`((packages (sample . ,(tool \
\"sample\" \"text\"))))")
                           'packages 'sample))))

(test-equal "options have a type, a default, and values from outside"
  '("one" "two" "3" #t #t)
  (with-workspace directory
    (flake! directory
            '((options (variant string "one" "Which one")
                       (count integer 1 "How many")))
            "`((packages (sample . ,(tool \"sample\"
  (if (= 1 (option in 'count))
      (option in 'variant)
      (number->string (option in 'count)))))))")
    (let ((sample (lambda options
                    (built (evaluate-flake %store directory
                                           'packages 'sample
                                           #:options options)))))
      (list (sample)
            (sample '(variant . "two"))
            ;; Text from a command line is converted.
            (sample '(count . "3"))
            (raises? (sample '(count . "many")))
            (raises? (sample '(absent . "x")))))))

(test-assert "what the flake prints is not taken for an answer"
  (with-workspace directory
    (derivation?
     (evaluate-flake %store
                     (flake! directory '()
                             "(display \"(done)\") (newline)"
                             "`((packages (sample . ,(tool \"sample\" \
\"text\"))))")
                     'packages 'sample))))

(test-assert "an error in the flake is an error here"
  (with-workspace directory
    (raises?
     (evaluate-flake %store
                     (flake! directory '() "(error \"the flake fails\")")
                     'packages 'sample))))

(test-equal "outputs of every kind are listed, lowered or not"
  '((packages sample #t) (checks works #t)
    (templates basic #f) (surprises thing #f))
  (with-workspace directory
    (map (match-lambda
           ((kind name derivation)
            (list kind name (->bool derivation))))
         (evaluate-flake-outputs
          %store
          (flake! directory '()
                  "(define sample (tool \"sample\" \"text\"))"
                  "`((packages (sample . ,sample))
  (checks (works . ,sample))
  (templates (basic . \"templates/basic\"))
  (surprises (thing . 42)))")))))

(test-equal "the commit of the tree is known when it has no changes"
  '(#t #f)
  (with-workspace directory
    (let* ((commit (commit! directory
                            `("flake.scm"
                              . ,(flake-text
                                  '()
                                  "`((packages (sample . ,(tool \"sample\"
  (or (self-revision in) \"changed\")))))"))))
           (sample (lambda ()
                     (built (evaluate-flake %store directory
                                            'packages 'sample))))
           (clean  (sample)))
      (write-file (string-append directory "/flake.scm")
                  (string-append (contents (string-append directory
                                                          "/flake.scm"))
                                 "\n"))
      (list (string=? commit clean)
            (string=? commit (sample))))))


;;;
;;; The flake's own files.
;;;

(define %with-source
  "`((packages (sample . ,(tool \"sample\" \"text\"
  #:source (source-set in \"one\" \"sub\")))))")

(test-equal "a source set changes with its files and with nothing else"
  '(#t #f)
  (with-workspace directory
    (commit! directory `("flake.scm" . ,(flake-text '() %with-source))
             '("one" . "1") '("two" . "2") '("sub/three" . "3"))
    (let* ((sample (lambda ()
                     (derivation-file-name
                      (evaluate-flake %store directory 'packages 'sample))))
           (first  (sample)))
      (write-file (string-append directory "/two") "changed")
      (let ((second (sample)))
        (write-file (string-append directory "/sub/three") "changed")
        (list (string=? first second)
              (string=? first (sample)))))))

(test-assert "a source set does not reach through a symbolic link"
  (with-workspace directory
    (let ((project (string-append directory "/project"))
          (outside (string-append directory "/outside")))
      (write-file (string-append outside "/secret") "secret")
      (mkdir-p project)
      (symlink outside (string-append project "/link"))
      (commit! project
               `("flake.scm"
                 . ,(flake-text
                     '()
                     "`((packages (sample . ,(tool \"sample\" \"text\"
  #:source (source-set in \"link/secret\")))))"))
               '("one" . "1"))
      (git project "add" "link")
      (raises? (evaluate-flake %store project 'packages 'sample)))))


;;;
;;; The cache.
;;;

(define (starts thunk)
  "Return the number of evaluators that THUNK starts."
  (let ((before (evaluator-starts)))
    (thunk)
    (- (evaluator-starts) before)))

(test-equal "an evaluation is cached until something it depends on changes"
  '(1 0 0 1 1 0)
  (with-workspace directory
    (flake! directory '((options (variant string "one" "Which one")))
            "`((packages (sample . ,(tool \"sample\" (option in \
'variant)))))")
    (let ((evaluate (lambda options
                      (lambda ()
                        ;; Forget what this process remembers: the next
                        ;; command is another process.
                        (hash-clear! %evaluations)
                        (evaluate-flake %store directory 'packages 'sample
                                        #:options options)))))
      (list (starts (evaluate))
            (starts (evaluate))
            ;; A file that is not tracked is not part of the flake.
            (begin
              (write-file (string-append directory "/untracked") "x")
              (starts (evaluate)))
            (starts (evaluate '(variant . "two")))
            (begin
              (write-file (string-append directory "/flake.scm")
                          (string-append
                           (contents (string-append directory "/flake.scm"))
                           "\n"))
              (starts (evaluate)))
            (starts (evaluate))))))

;;;
;;; Inputs.
;;;

(define (lock! directory)
  "Lock the flake in DIRECTORY and commit the lock; return DIRECTORY."
  (lock-flake directory)
  (commit! directory
           `("flake.lock"
             . ,(contents (string-append directory "/flake.lock"))))
  directory)

(define (child! directory)
  "Make DIRECTORY a flake whose package 'child' holds the text of its file
'version'."
  (commit! directory
           `("flake.scm"
             . ,(flake-text
                 '()
                 "`((packages (child . ,(tool \"child\"
  (call-with-input-file (self-file in \"version\")
    (@ (ice-9 textual-ports) get-string-all))))))"))
           '("version" . "1"))
  directory)

(define (parent! directory child name)
  "Make DIRECTORY a flake with the input 'child' from CHILD, whose package
NAME holds the file name of the child's package."
  (flake! directory
          `((inputs (child (git ,child))))
          (format #f "`((packages (~a . ,(tool ~s
  (input-output in 'child 'packages 'child)))))"
                  name name)))

(define (child-of derivation)
  "Return the text of the child that the parent DERIVATION was built with."
  (contents (built derivation)))

(test-equal "an input that is a flake is evaluated with its own inputs"
  "1"
  (with-workspace directory
    (let* ((child  (child! (string-append directory "/child")))
           (parent (parent! (string-append directory "/parent") child
                            "parent"))
           (root   (lock!
                    (flake! (string-append directory "/root")
                            `((inputs (parent (git ,parent))))
                            "`((packages (parent . ,(input-output in \
'parent 'packages 'parent))))"))))
      (child-of (evaluate-flake %store root 'packages 'parent)))))

(test-equal "two pinned inputs keep two revisions of a third under one root"
  '("1" "2" "3" "3")
  (with-workspace directory
    (let* ((child (child! (string-append directory "/child")))
           (left  (lock! (parent! (string-append directory "/left") child
                                  "left")))
           (_     (commit! child '("version" . "2")))
           (right (lock! (parent! (string-append directory "/right") child
                                  "right")))
           (_     (commit! child '("version" . "3")))
           (root  (lambda (name . fields)
                    (lock!
                     (flake! (string-append directory "/" name)
                             `((inputs (child (git ,child))
                                       (left (git ,left) ,@fields)
                                       (right (git ,right) ,@fields)))
                             "`((packages
  (left . ,(input-output in 'left 'packages 'left))
  (right . ,(input-output in 'right 'packages 'right))))"))))
           (pinned (root "pinned" '(pinned)))
           (shared (root "shared")))
      (list (child-of (evaluate-flake %store pinned 'packages 'left))
            (child-of (evaluate-flake %store pinned 'packages 'right))
            ;; By default there is one child: the root's.
            (child-of (evaluate-flake %store shared 'packages 'left))
            (child-of (evaluate-flake %store shared 'packages 'right))))))

(test-equal "replacing the input of one input changes that one only"
  '(#f #t "other")
  (with-workspace directory
    (let* ((child (child! (string-append directory "/child")))
           (other (child! (string-append directory "/other")))
           (_     (commit! other '("version" . "other")))
           (left  (parent! (string-append directory "/left") child "left"))
           (right (parent! (string-append directory "/right") child "right"))
           (root  (lambda (name . fields)
                    (lock!
                     (flake! (string-append directory "/" name)
                             `((inputs (left (git ,left) ,@fields)
                                       (right (git ,right))))
                             "`((packages
  (left . ,(input-output in 'left 'packages 'left))
  (right . ,(input-output in 'right 'packages 'right))))"))))
           (plain    (root "plain"))
           (replaced (root "replaced" `(inputs (child (git ,other)))))
           (name     (lambda (root output)
                       (derivation-file-name
                        (evaluate-flake %store root 'packages output)))))
      (list (string=? (name plain 'left) (name replaced 'left))
            (string=? (name plain 'right) (name replaced 'right))
            (child-of (evaluate-flake %store replaced 'packages 'left))))))

(test-equal "an input gets the options that the flake sets on it"
  '("default" "set")
  (with-workspace directory
    (let* ((child (flake! (string-append directory "/child")
                          '((options (variant string "default" "Which")))
                          "`((packages (child . ,(tool \"child\" (option in \
'variant)))))"))
           (root  (lambda (name . fields)
                    (lock!
                     (flake! (string-append directory "/" name)
                             `((inputs (child (git ,child) ,@fields)))
                             "`((packages (child . ,(input-output in 'child \
'packages 'child))))")))))
      (list (built (evaluate-flake %store (root "plain") 'packages 'child))
            (built (evaluate-flake %store
                                   (root "set" '(options (variant "set")))
                                   'packages 'child))))))

(test-equal "the tree of an input is a directory in the store"
  "data"
  (with-workspace directory
    (let ((tree (string-append directory "/tree")))
      (commit! tree '("file" . "data"))
      (built
       (evaluate-flake
        %store
        (lock!
         (flake! (string-append directory "/root")
                 `((inputs (tree (git ,tree) (as source))))
                 "`((packages (sample . ,(tool \"sample\"
  (call-with-input-file (string-append (input-source in 'tree) \"/file\")
    (@ (ice-9 textual-ports) get-string-all))))))"))
        'packages 'sample)))))

(test-equal "the modules of a library input are on the load path"
  '("from the library" "from the flake")
  (with-workspace directory
    (let ((library (string-append directory "/library"))
          (root    (string-append directory "/root")))
      (commit! library
               '(".guix-channel" . "(channel (version 0) (directory \"src\"))")
               '("src/library/words.scm"
                 . "(define-module (library words) #:export (word))
(define word \"from the library\")"))
      (commit! root
               `("flake.scm"
                 . ,(flake-text
                     `((modules "scheme")
                       (inputs (library (git ,library))))
                     "(use-modules (library words) (own words))"
                     "`((packages (theirs . ,(tool \"theirs\" word))
  (mine . ,(tool \"mine\" own-word))))"))
               '("scheme/own/words.scm"
                 . "(define-module (own words) #:export (own-word))
(define own-word \"from the flake\")"))
      (lock! root)
      (list (built (evaluate-flake %store root 'packages 'theirs))
            (built (evaluate-flake %store root 'packages 'mine))))))

;;;
;;; Overrides.
;;;

(define (override-flake overrides)
  ;; 'wrapper' is built on 'original'; 'consumer' uses 'original'; 'direct'
  ;; names 'wrapper' itself.
  (flake-text
   '()
   "(define (text-of package)
  #~(call-with-input-file #$package (@ (ice-9 rdelim) read-line)))"
   "(define original (tool \"original\" \"original\"))"
   "(define wrapper
  (tool \"wrapper\" #~(string-append \"wrapped \" #$(text-of original))))"
   "(define consumer
  (tool \"consumer\" #~(string-append \"uses \" #$(text-of original))))"
   "(define direct
  (tool \"direct\" #~(string-append \"names \" #$(text-of wrapper))))"
   (string-append "`((packages (original . ,original) (wrapper . ,wrapper)
  (consumer . ,consumer) (direct . ,direct))
  (overrides " overrides "))")))

(test-equal "an override replaces a package wherever it is lowered"
  '("uses original"
    "uses wrapped original" "wrapped original" "names wrapped original"
    "wrapped original" #t)
  (with-workspace directory
    (let* ((plain   (string-append directory "/plain"))
           (changed (string-append directory "/changed"))
           (output  (lambda (flake name)
                      (evaluate-flake %store flake 'packages name))))
      (commit! plain `("flake.scm" . ,(override-flake "")))
      (commit! changed
               `("flake.scm"
                 . ,(override-flake
                     "(wrap . ,(override original wrapper))")))
      (list (built (output plain 'consumer))      ;the control
            (built (output changed 'consumer))
            ;; The wrapper's own original stays, however it is reached.
            (built (output changed 'wrapper))
            (built (output changed 'direct))
            (built (output changed 'original))
            ;; There is one wrapper, not one per way to it.
            (->bool
             (member (derivation-file-name (output changed 'wrapper))
                     (map derivation-input-path
                          (derivation-inputs
                           (output changed 'consumer)))))))))

;;;
;;; The Guix that evaluates.
;;;

(define %stand-ins (make-hash-table))

(define (stand-in-guix version)
  "Return a store directory that stands for the Guix built from the commit
VERSION, as 'guix time-machine' would have it: its 'bin/guix' is the Guix
under test, and says it is VERSION."
  (or (hash-ref %stand-ins version)
      (call-with-temporary-directory
       (lambda (directory)
         (let ((guix (string-append directory "/bin/guix")))
           (mkdir-p (dirname guix))
           (call-with-output-file guix
             (lambda (port)
               (format port "#!~a --no-auto-compile~%!#~%"
                       (@ (guix config) %guile))
               (write `(begin
                         (set! %load-path
                               (append ',%load-path %load-path))
                         (set! %load-compiled-path
                               (append ',%load-compiled-path
                                       %load-compiled-path))
                         (module-set! (resolve-module '(guix config))
                                      '%guix-version ,version)
                         (apply (@ (guix ui) guix-main) (command-line)))
                      port)))
           (chmod guix #o755)
           (let ((item (add-to-store %store
                                     (string-append "stand-in-"
                                                    (string-take version 7))
                                     #t "sha256" directory)))
             (hash-set! %stand-ins version item)
             item))))))

(define (call-with-stand-ins proc)
  "Call PROC with a procedure that returns the lists of channels that were
to be built, while a stand-in takes the place of each."
  (let ((asked '()))
    (parameterize ((%locked-guix-directory
                    (lambda (store channels)
                      (set! asked (cons channels asked))
                      (stand-in-guix (channel-commit (first channels))))))
      (proc (lambda () (reverse asked))))))

(define (channel-summary channel)
  (list (channel-name channel) (channel-url channel)
        (channel-commit channel)))

(test-assert "the input named 'guix' is built, and it evaluates the flake"
  (with-workspace directory
    (let* ((guix    (string-append directory "/guix"))
           (library (string-append directory "/library"))
           (commit  (commit! guix
                             '(".guix-channel" . "(channel (version 0))")))
           (other   (commit! library
                             '(".guix-channel" . "(channel (version 0))")))
           (project (lock!
                     (flake! (string-append directory "/project")
                             `((inputs (guix (git ,guix))
                                       (library (git ,library))))
                             "`((packages (sample . ,(tool \"sample\" \
\"text\"))))"))))
      (call-with-stand-ins
       (lambda (asked)
         (and (string=? "text"
                        (built (evaluate-flake %store project
                                               'packages 'sample)))
              ;; What was to be built: that commit of Guix, with the
              ;; library as a channel next to it.
              (equal? `(((guix ,guix ,commit) (library ,library ,other)))
                      (map (lambda (channels)
                             (map channel-summary channels))
                           (asked)))))))))

(test-assert "a Guix that is not the pinned commit is refused"
  (with-workspace directory
    (let* ((guix    (string-append directory "/guix"))
           (commit  (commit! guix
                             '(".guix-channel" . "(channel (version 0))")))
           (project (lock!
                     (flake! (string-append directory "/project")
                             `((inputs (guix (git ,guix))))
                             "`((packages (sample . ,(tool \"sample\" \
\"text\"))))"))))
      (parameterize ((%locked-guix-directory
                      (lambda (store channels)
                        (stand-in-guix (make-string 40 #\0)))))
        (raises? (evaluate-flake %store project 'packages 'sample))))))

(test-equal "a pinned input is evaluated by its own Guix"
  '(#t #t)
  (with-workspace directory
    (let* ((guix  (string-append directory "/guix"))
           (old   (commit! guix
                           '(".guix-channel" . "(channel (version 0))")))
           (child (lock!
                   (flake! (string-append directory "/child")
                           `((inputs (guix (git ,guix))))
                           "`((packages (child . ,(tool \"child\" \
\"text\"))))")))
           (new   (commit! guix '("later" . "x")))
           (root  (lambda (name . fields)
                    (lock!
                     (flake! (string-append directory "/" name)
                             `((inputs (guix (git ,guix))
                                       (child (git ,child) ,@fields)))
                             "`((packages (child . ,(input-output in 'child \
'packages 'child))))"))))
           (commits (lambda (root)
                      ;; The commits of Guix that were to be built, for the
                      ;; flake and then for its input.
                      (call-with-stand-ins
                       (lambda (asked)
                         (evaluate-flake %store root 'packages 'child)
                         (map (lambda (channels)
                                (channel-commit (first channels)))
                              (asked)))))))
      (list (equal? (list new old) (commits (root "pinned" '(pinned))))
            ;; By default the input follows the flake's Guix.
            (equal? (list new new) (commits (root "shared")))))))

(define (record-predicate-works? module constructor predicate)
  ;; The record predicates are macros in Guile. The evaluator must evaluate
  ;; their names in their modules, not depend on SRFI-9's generated names.
  (let ((object (if (eq? constructor 'hurd64-barebones-os)
                    (module-ref (resolve-module '(gnu system images hurd))
                                constructor)
                    (eval (list constructor) (resolve-module module)))))
        (procedure (@@ (guix flakes evaluator) optional-binding)
                    module predicate)))
    (and (procedure? procedure) (procedure object))))

(test-assert "the operating-system predicate is resolved from its module"
  (record-predicate-works? '(gnu system) 'hurd64-barebones-os 'operating-system?))

(test-assert "the image predicate is resolved from its module"
  (record-predicate-works? '(gnu image) 'image 'image?))

(test-assert "the home-environment predicate is resolved from its module"
  (record-predicate-works? '(gnu home) 'home-environment 'home-environment?))

(test-end "flake-eval")
