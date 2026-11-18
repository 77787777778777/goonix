;;; goonix: data-first flake commands.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.

(define-module (guix scripts flake)
  #:use-module (guix flakes)
  #:autoload   (guix flakes evaluate) (evaluate-flake evaluate-flake-outputs)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix diagnostics)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:export (guix-flake))

(define (show-help)
  (display (G_ "Usage: guix flake COMMAND [OPTION...] [DIRECTORY]
Lock, inspect, and evaluate a goonix flake.

  lock                    resolve inputs without running project code
  update [INPUT]          update the lock, or only INPUT
  metadata                print the locked dependency graph as data
  show                    list declared outputs
  check                   build the flake's checks
  init                    create a flake.scm

  --override-input=NAME=SPEC
                          use a local input for this invocation only
  --option=NAME=VALUE    set a declared evaluation option
  --system=SYSTEM         evaluate for SYSTEM
  --target=TARGET         cross evaluate for TARGET
  -h, --help              display this help and exit
  -V, --version           display version information and exit\n")))

(define (option-value string)
  (let ((equal (string-index string #\=)))
    (unless equal (leave (G_ "expected NAME=VALUE, got '~a'~%") string))
    (values (string->symbol (string-take string equal))
            (string-drop string (+ equal 1)))))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda (opt name arg result)
                  (leave-on-EPIPE (show-help))
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda _ (show-version-and-exit "guix flake")))
        (option '("override-input") #t #f
                (lambda (opt name arg result)
                  (let-values (((name value) (option-value arg)))
                    (alist-cons 'override (cons name value) result))))
        (option '("option") #t #f
                (lambda (opt name arg result)
                  (let-values (((name value) (option-value arg)))
                    (alist-cons 'option (cons name value) result))))
        (option '("system") #t #f
                (lambda (opt name arg result) (alist-cons 'system arg result)))
        (option '("target") #t #f
                (lambda (opt name arg result) (alist-cons 'target arg result)))))

(define (parse-override value)
  (let ((data (read-data-string value)))
    (match data
      ((('local path)) (list (list 'local path)))
      ((('git url) rest ...) (cons (list 'git url) rest))
      (((or 'local 'git) source rest ...) (cons (list (car data) source) rest))
      (_ (leave (G_ "invalid local input override: ~s~%") value)))))

(define (directory-of options)
  (match (reverse (filter-map (match-lambda (('argument . value) value) (_ #f))
                             options))
    (() (getcwd))
    ((directory) directory)
    (_ (leave (G_ "only one flake directory may be specified~%")))))

(define-command (guix-flake . args)
  (category development)
  (synopsis "lock, inspect, and evaluate goonix flakes")
  (match args
    (() (show-help) (exit 1))
    (((or "--help" "-h") . _) (show-help) (exit 0))
    ((command arguments ...)
     (let* ((options (parse-command-line arguments %options (list '())
                                         #:build-options? #f))
            (directory (directory-of options))
            (overrides
             (map (match-lambda
                              ((name . value) (cons name (parse-override value))))
                            (reverse (filter-map
                                      (match-lambda (('override . pair) pair)
                                                    (_ #f)) options)))))
       (case (string->symbol command)
         ((lock)
          (lock-flake directory #:local-overrides overrides)
          (format #t "~a~%" (string-append directory "/flake.lock")))
         ((update)
          (update-flake directory (match arguments (() #t) ((input) input)
                                    (_ #t))))
         ((metadata)
          (pretty-print (flake-metadata (effective-flake-lock directory
                                                              #:overrides overrides))))
         ((show)
          (with-store store
            (for-each
             (match-lambda
               ((kind name path)
                (format #t "~a ~a ~a~%" kind name path)))
             (evaluate-flake-outputs store directory
                                    #:options (filter-map
                                               (match-lambda
                                                 (('option . pair) pair)
                                                 (_ #f)) options)
                                    #:overrides overrides))))
         ((check)
          (with-store store
            (build-derivations
             store
             (map (lambda (entry)
                    (read-derivation-from-file (caddr entry)))
                  (filter (lambda (entry) (eq? 'checks (car entry)))
                          (evaluate-flake-outputs store directory))))))
         (else (leave (G_ "unknown flake command '~a'~%") command)))))))
