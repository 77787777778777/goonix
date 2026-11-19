;;; goonix: package and derivation addressing policy for mixed Guix.
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
  #:use-module (guix build-system)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix ca)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%package-addressing-policy
            %upstream-package?
            upstream-package?
            %package-addressing
            upstream-package-set
            package-addressing
            package-addressing-key
            lower-bag-with-addressing
            gexp-with-addressing
            gexp-addressing))

;;; Commentary:
;;;
;;; The package mark selects only the requested kind for that package's own
;;; bag-named gexp.  Nested derivations do not inherit it.  The final kind is
;;; chosen after gexp inputs and reference lists have been lowered: under
;;; mixed an input-addressed request whose inputs are not all input-addressed
;;; becomes content-addressed.  A placeholder in a reference list has the same
;;; effect, even when the object is not a derivation input.  Thus origins,
;;; computed files, graphs, profiles and packages obey the same rule.

(define %package-addressing-policy
  (make-parameter 'input))

(define %guix-source-tree
  (dirname (dirname (search-path %load-path "guix/packages.scm"))))

(define %upstream-source-file?
  (let ((cache (make-hash-table)))
    (lambda (file)
      (or (hash-ref cache file)
          (let* ((absolute (if (absolute-file-name? file)
                               file
                               (string-append %guix-source-tree "/" file)))
                 (gnu (string-append %guix-source-tree "/gnu/"))
                 (guix (string-append %guix-source-tree "/guix/"))
                 (result (and (or (string-prefix? gnu absolute)
                                  (string-prefix? guix absolute))
                              (file-exists? absolute))))
            (hash-set! cache file result)
            result)))))

(define (upstream-package? package)
  "Whether PACKAGE was defined in the running Guix's guix/ or gnu/ tree."
  (let ((location (package-location package)))
    (and location (%upstream-source-file? (location-file location)))))

(define %upstream-package?
  ;; Location marks upstream definitions, including runtime-generated ones;
  ;; inherited records receive their new definition's location.
  (make-parameter upstream-package?))

(define %package-addressing
  (make-parameter 'input))

(define %package-derivation-name
  (make-parameter #f))

(define (upstream-package-set packages)
  "Return a predicate marking precisely the package objects in PACKAGES."
  (lambda (package)
    (set-contains? packages package)))

(define (package-addressing package)
  "Return the kind to request for PACKAGE's own derivation."
  (cond ((eq? 'opaque (assq-ref (package-properties package) 'self-references))
         'resolved)
        ((memq (%package-addressing-policy) '(content full)) 'content)
        ((eq? (%package-addressing-policy) 'mixed)
         (if ((%upstream-package?) package) 'input 'content))
        (else 'input)))

(define (package-addressing-key)
  "Return the current mode and marker for the package derivation cache."
  (list (%package-addressing-policy) (%upstream-package?)))

(define (lower-bag-with-addressing lower bag . arguments)
  "Apply the package's own addressing around its bag build, after inputs."
  (let ((context (and (pair? arguments) (car arguments))))
    (if (package? context)
        (state-parameterize ((%package-addressing (package-addressing context))
                             (%package-derivation-name (bag-name bag)))
          (apply lower bag arguments))
        (apply lower bag arguments))))

(define (keyword-argument arguments keyword)
  (let loop ((rest arguments))
    (and (pair? rest)
         (if (eq? (car rest) keyword)
             (cadr rest)
             (loop (cdr rest))))))

(define (gexp-with-addressing lower name exp . arguments)
  "Apply LOWER with the requested kind for this derivation only."
  (let* ((package-name (%package-derivation-name))
         (requested (cond ((keyword-argument arguments #:addressing) => identity)
                          ((and package-name (string=? name package-name))
                           (%package-addressing))
                          ((memq (%package-addressing-policy) '(content full))
                           'content)
                          ((eq? (%package-addressing-policy) 'mixed) 'mixed)
                          (else 'input))))
    (state-parameterize ((%package-derivation-name #f))
      (apply lower name exp
             (append arguments (list #:addressing requested))))))

(define (input-addressed-derivation? input)
  "Whether a lowered derivation input has no CA addressing marker."
  (not (assoc-ref (derivation-builder-environment-vars
                   (derivation-input-derivation input))
                  "goonixAddressing")))

(define (gexp-inputs-input-addressed? guile inputs graphs)
  (and (every input-addressed-derivation? (cons guile inputs))
       (or (not graphs)
           (every (match-lambda
                    ((_ . inputs)
                     (every input-addressed-derivation?
                            (filter derivation-input? inputs))))
                  graphs))))

(define (references-input-addressed? references)
  (or (not references)
      (every (lambda (path) (not (placeholder-path? path))) references)))

(define (gexp-addressing requested guile inputs graphs allowed disallowed)
  "Choose REQUESTED using the actual lowered inputs and reference paths."
  (if (not (eq? (%package-addressing-policy) 'mixed))
      requested
      (if (and (memq requested '(input mixed))
               (or (not (gexp-inputs-input-addressed? guile inputs graphs))
                   (not (references-input-addressed? allowed))
                   (not (references-input-addressed? disallowed))))
          'content
          (if (eq? requested 'mixed) 'input requested))))
