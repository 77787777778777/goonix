;;; goonix: package addressing policy for the mixed Guix transition.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix ca policy)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix sets)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%package-addressing-policy
            %upstream-packages
            %package-addressing
            upstream-package-set
            upstream-package-closure
            package-addressing
            package-addressing-key
            lower-package-with-addressing
            gexp-with-addressing))

;;; Commentary:
;;;
;;; In the default input mode, package lowering is exactly upstream's.  In
;;; mixed mode an identity-marked upstream package stays input-addressed if
;;; and only if all of its package inputs are upstream's too.  An inherited
;;; or modified package is a new object and is therefore content-addressed.
;;; The explicit opaque property keeps its resolved addressing in every mode.
;;;
;;; The addressing parameter is installed by 'lower-package-with-addressing' around
;;; the store-monadic value, not after it runs.  The build system and its
;;; gexps therefore see the choice while constructing their builder, and
;;; dependent packages see the addressing of their inputs before choosing
;;; their own.

(define %package-addressing-policy
  (make-parameter 'input))

(define-record-type <upstream-packages>
  (upstream-packages key packages)
  upstream-packages?
  (key upstream-packages-key)
  (packages upstream-packages-list))

(define %upstream-packages
  ;; An origin object has a stable identity, unlike a fresh set->list result.
  (make-parameter (upstream-packages 0 '())))

(define %package-addressing
  (make-parameter 'input))

(define (upstream-package-set packages)
  "Return an origin marker for a set of upstream package objects."
  (upstream-packages (object-address packages) (set->list packages)))

(define* (upstream-package-closure packages #:optional (system (%current-system)))
  "Return the bag closure of PACKAGES as an upstream origin set.

Build systems add implicit inputs while making bags; they are part of the
upstream graph even though they are not in the package's declared inputs."
  (let ((cache (make-weak-key-hash-table)))
    (let visit ((pending packages))
      (for-each (lambda (package)
                  (unless (hashq-ref cache package)
                    (hashq-set! cache package #t)
                    (visit (filter-map (match-lambda
                                         ((_ (? package? input) . _) input)
                                         (_ #f))
                                       (bag-direct-inputs
                                        (package->bag package system))))))
                pending))
    (upstream-package-set
     (list->setq (hash-map->list (lambda (package _) package) cache)))))

(define (package-properties* package)
  "Return PACKAGE's properties without expanding its record transformer here."
  ((@@ (guix packages) %package-properties-procedure) package))

(define (package-opaque? package)
  (eq? 'opaque (assq-ref (package-properties* package) 'self-references)))

(define (package-input-addressed? package)
  "Whether PACKAGE and every package input retain upstream addressing.

One memo table covers the entire traversal, including bootstrap cycles."
  (let ((cache (make-weak-key-hash-table)))
    (let visit ((package package))
      (or (hashq-ref cache package)
          (and (memq package (upstream-packages-list (%upstream-packages)))
               (begin
                 (hashq-set! cache package #t)
                 (let ((result
                        (every (match-lambda
                                 ((_ (? package? input) . _)
                                  (parameterize
                                      ((%package-addressing
                                        (if (package-opaque? input)
                                            'resolved 'input)))
                                    (visit input)))
                                 (_ #t))
                               (bag-direct-inputs (package->bag package)))))
                   (hashq-set! cache package result)
                   result)))))))

(define (package-addressing package)
  "Return the addressing to use while lowering PACKAGE."
  (cond ((eq? 'opaque (assq-ref (package-properties* package) 'self-references))
         'resolved)
        ((memq (%package-addressing-policy) '(content full))
         'content)
        ((eq? (%package-addressing-policy) 'mixed)
         (if (package-input-addressed? package) 'input 'content))
        (else 'input)))

(define (package-addressing-key)
  "Return a key that changes whenever the mode or upstream origin set does."
  (list (%package-addressing-policy)
        (upstream-packages-key (%upstream-packages))))

(define (lower-package-with-addressing lower package . arguments)
  "Lower PACKAGE with its policy bound during construction and execution."
  (state-parameterize ((%package-addressing (package-addressing package)))
    (apply lower package arguments)))

(define (keyword-argument arguments keyword)
  "Return the value following KEYWORD in ARGUMENTS, or #f when absent."
  (let loop ((rest arguments))
    (and (pair? rest)
         (if (eq? (car rest) keyword)
             (cadr rest)
             (loop (cdr rest))))))

(define (gexp-with-addressing lower name exp . arguments)
  "Apply LOWER to a gexp, using the package's addressing when appropriate."
  (apply lower name exp
         (if (or (keyword-argument arguments #:addressing)
                 (eq? (%package-addressing) 'input))
             arguments
             (append arguments (list #:addressing (%package-addressing))))))
