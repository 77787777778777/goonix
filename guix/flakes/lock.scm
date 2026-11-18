;;; goonix: flake lock data and deterministic serialization.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix flakes lock)
  #:use-module ((guix flakes syntax)
                #:select (read-data-file read-data-string flake-input->sexp))
  #:use-module (guix records)
  #:use-module (guix base32)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (flake-lock? make-flake-lock flake-lock flake-lock-version flake-lock-inputs
            locked-input? make-locked-input locked-input locked-input-name
            locked-input-type locked-input-url locked-input-path
            locked-input-branch locked-input-tag locked-input-commit
            locked-input-tree-hash locked-input-as locked-input-worktree?
            locked-input-override?
            locked-input-authentication locked-input-introduction
            locked-input-edges locked-input-options locked-input-checkout
            lock-edge? make-lock-edge lock-edge lock-edge-from lock-edge-name
            lock-edge-kind lock-edge-target
            read-flake-lock read-flake-local write-flake-lock
            flake-lock->sexp))

(define-record-type* <flake-lock> flake-lock %make-flake-lock
  flake-lock?
  (version flake-lock-version (default 1))
  (inputs  flake-lock-inputs (default '())))

(define-record-type* <locked-input> locked-input make-locked-input
  locked-input?
  (name       locked-input-name)
  (type       locked-input-type)
  (url        locked-input-url (default #f))
  (path       locked-input-path (default #f))
  (branch     locked-input-branch (default #f))
  (tag        locked-input-tag (default #f))
  (commit     locked-input-commit (default #f))
  (tree-hash  locked-input-tree-hash)
  (as         locked-input-as (default 'flake))
  (worktree?  locked-input-worktree? (default #f))
  (override?  locked-input-override? (default #f))
  (authentication locked-input-authentication (default 'pinned-by-hash))
  (introduction locked-input-introduction (default #f))
  (edges      locked-input-edges (default '()))
  (options    locked-input-options (default '()))
  ;; The checkout is runtime information.  It is deliberately not serialized.
  (checkout   locked-input-checkout (default #f)))

(define-record-type* <lock-edge> lock-edge make-lock-edge
  lock-edge?
  (from       lock-edge-from)
  (name       lock-edge-name)
  (kind       lock-edge-kind)
  (target     lock-edge-target (default #f)))

(define (invalid datum message)
  (error (format #f "invalid flake lock: ~a: ~s" message datum)))

(define (field properties key default)
  (match (filter (match-lambda (((? (lambda (x) (eq? x key))) . _) #t)
                               (_ #f))
                 properties)
    (() default)
    (((_ value)) value)
    (_ (invalid properties "duplicate field"))))

(define (sexp->edge datum)
  (match datum
    (('edge properties ...)
     (let ((from (field properties 'from #f))
           (name (field properties 'name #f))
           (kind (field properties 'kind #f))
           (target (field properties 'target #f)))
       (unless (and (symbol? from) (symbol? name)
                    (memq kind '(direct followed pinned replaced same-as)))
         (invalid datum "invalid edge"))
       (lock-edge (from from) (name name) (kind kind) (target target))))
    (_ (invalid datum "expected edge"))))

(define (sexp->locked-input datum)
  (match datum
    (((? symbol? name) properties ...)
     (let* ((type (field properties 'type #f))
            (commit (field properties 'commit #f))
            (hash (field properties 'tree-hash #f))
            (auth (field properties 'authentication 'pinned-by-hash))
            (edges (field properties 'edges '())))
       (unless (memq type '(git local))
         (invalid datum "unknown input type"))
       (unless (and (string? commit)
                    (or (= 40 (string-length commit))
                        (= 64 (string-length commit)))
                    (string-every char-set:hex-digit commit))
         (invalid datum "commit must be a full hexadecimal commit ID"))
       (unless (and (string? hash)
                    (false-if-exception (nix-base32-string->bytevector hash)))
         (invalid datum "tree-hash must be nix-base32 SHA-256"))
       (unless (memq auth '(authenticated pinned-by-hash))
         (invalid datum "unknown authentication status"))
       (locked-input
        (name name) (type type)
        (url (field properties 'url #f))
        (path (field properties 'path #f))
        (branch (field properties 'branch #f))
        (tag (field properties 'tag #f))
        (commit commit) (tree-hash hash)
        (as (field properties 'as 'flake))
        (worktree? (field properties 'worktree #f))
        (override? (field properties 'override #f))
        (authentication auth)
        (introduction (field properties 'introduction #f))
        (edges (map sexp->edge edges))
        (options (field properties 'options '())))))
    (_ (invalid datum "expected a named input"))))

(define (sexp->flake-lock datum)
  (match datum
    (('flake-lock properties ...)
     (let ((version (field properties 'version #f))
           (inputs (field properties 'inputs '())))
       (unless (eqv? version 1)
         (invalid datum "unsupported lock version"))
       (flake-lock (version version)
                   (inputs (map sexp->locked-input inputs)))))
    (_ (invalid datum "expected (flake-lock (version 1) ...)"))))

(define (read-flake-lock file)
  "Read and validate FILE without evaluating any of its contents."
  (sexp->flake-lock (read-data-file file)))

(define (read-flake-local file)
  "Read local overrides as a list of validated <flake-input> records."
  (match (read-data-file file)
    (('flake-local ('version 1) ('inputs inputs ...))
     (let ((parse (module-ref (resolve-interface '(guix flakes syntax))
                              'sexp->input)))
       (map parse inputs)))
    (_ (invalid file "expected (flake-local (version 1) (inputs ...))"))))

(define (edge->sexp edge)
  `(edge (from ,(lock-edge-from edge))
         (name ,(lock-edge-name edge))
         (kind ,(lock-edge-kind edge))
         ,@(if (lock-edge-target edge)
               `((target ,(lock-edge-target edge)))
               '())))

(define (locked-input->sexp input)
  `(,(locked-input-name input)
    (type ,(locked-input-type input))
    ,@(if (locked-input-url input) `((url ,(locked-input-url input))) '())
    ,@(if (locked-input-path input) `((path ,(locked-input-path input))) '())
    (commit ,(locked-input-commit input))
    (tree-hash ,(locked-input-tree-hash input))
    ,@(if (locked-input-branch input)
          `((branch ,(locked-input-branch input))) '())
    ,@(if (locked-input-tag input) `((tag ,(locked-input-tag input))) '())
    ,@(if (not (eq? 'flake (locked-input-as input)))
          `((as ,(locked-input-as input))) '())
    ,@(if (locked-input-worktree? input) '((worktree)) '())
    (authentication ,(locked-input-authentication input))
    ,@(if (locked-input-introduction input)
          `((introduction ,(locked-input-introduction input))) '())
    ,@(if (null? (locked-input-options input)) '()
          `((options ,(locked-input-options input))))
    ,@(if (null? (locked-input-edges input)) '()
          `((edges ,(map edge->sexp (locked-input-edges input)))))))

(define (flake-lock->sexp lock)
  `(flake-lock
    (version ,(flake-lock-version lock))
    (inputs ,(map locked-input->sexp (flake-lock-inputs lock)))))

(define (write-flake-lock lock file)
  "Atomically write LOCK to FILE in canonical, reproducible S-expression form."
  (let ((temporary (string-append file ".tmp")))
    (call-with-output-file temporary
      (lambda (port)
        (display ";; goonix: generated; do not edit.\n" port)
        (pretty-print (flake-lock->sexp lock) port
                      #:max-expr-width 78)
        (newline port)))
    (rename-file temporary file)
    file))
