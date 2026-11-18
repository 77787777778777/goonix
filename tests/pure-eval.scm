;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 the Guix authors

(define-module (test-pure-eval)
  #:use-module (guix pure-eval)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix tests)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (guix build utils)
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:use-module (srfi srfi-64))

(test-begin "pure-eval")

(define %store (open-connection-for-tests))

(test-assert "pure evaluation is available in the test environment"
  (pure-eval-supported?))

(test-equal "tracked project files are imported"
  '("tracked.scm")
  (call-with-temporary-directory
   (lambda (directory)
     (let ((project (string-append directory "/project")))
       (mkdir project)
       (mkdir (string-append project "/.git"))
       (call-with-output-file (string-append project "/tracked.scm")
         (lambda (port) (display "#t\n" port)))
       (call-with-output-file (string-append project "/untracked.scm")
         (lambda (port) (display "#t\n" port)))
       (with-directory-excursion project
         (system* "git" "init" "-q")
         (system* "git" "add" "tracked.scm"))
       (let ((imported (import-tracked-tree %store project)))
         (and (file-exists? (string-append imported "/tracked.scm"))
              (not (file-exists? (string-append imported "/untracked.scm")))
              (scandir imported (lambda (file)
                                  (not (member file '("." "..")))))))))))

(test-equal "container returns derivation paths"
  '("/gnu/store/example.drv")
  (call-with-pure-eval %store '()
    (const #t)
    #:evaluate (lambda (store)
                 (list "/gnu/store/example.drv"))))

(test-assert "ambient environment is not inherited"
  (begin
    (setenv "GOONIX_PURE_EVAL_SECRET" "not visible")
    (call-with-pure-eval %store '()
      (const #t)
      #:evaluate (lambda (store)
                   (not (getenv "GOONIX_PURE_EVAL_SECRET"))))))

(test-assert "host working directory content is not visible"
  (call-with-pure-eval %store '()
    (const #t)
    #:evaluate (lambda (store)
                 (not (file-exists? "/etc/nixos/flake.nix")))))

(test-assert "network namespace has no external resolver"
  (call-with-pure-eval %store '()
    (const #t)
    #:evaluate (lambda (store)
                 (catch #t
                   (lambda ()
                     (getaddrinfo "www.gnu.org" "80")
                     #f)
                   (const #t)))))

(test-end)
