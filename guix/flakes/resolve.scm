;;; goonix: lock resolution, local overrides, and metadata.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix flakes resolve)
  #:use-module ((guix flakes syntax) #:select (sexp->input read-flake read-data-file
                        flake-inputs flake-input-name flake-input-type
                        flake-input-url flake-input-path flake-input-branch
                        flake-input-tag flake-input-commit flake-input-as
                        flake-input-worktree? flake-input-pinned?
                        flake-input-follows flake-input-overrides
                        flake-input-options flake-input-introduction))
  #:use-module (guix flakes lock)
  #:use-module (guix flakes checkout)
  #:use-module ((guix channels)
                #:select (channel channel-name channel-url channel-commit
                          channel-branch channel-introduction
                          authenticate-channel make-channel-introduction
                          openpgp-fingerprint))
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (lock-flake update-flake verify-flake-lock
            read-local-overrides effective-flake-lock
            flake-metadata))

(define (lock-file directory)
  (string-append directory "/flake.lock"))

(define (local-file directory)
  (string-append directory "/flake.local.scm"))

(define (same-source? input locked)
  (and (eq? (flake-input-type input) (locked-input-type locked))
       (equal? (flake-input-url input) (locked-input-url locked))
       (equal? (flake-input-path input) (locked-input-path locked))
       (equal? (flake-input-branch input) (locked-input-branch locked))
       (equal? (flake-input-tag input) (locked-input-tag locked))
       (equal? (flake-input-as input) (locked-input-as locked))
       (or (not (flake-input-commit input))
           (equal? (flake-input-commit input) (locked-input-commit locked)))))

(define (authentication input snapshot)
  (let ((introduction (flake-input-introduction input)))
    (if (and introduction (eq? 'git (flake-input-type input)))
        (let ((chan (channel (name (flake-input-name input))
                             (url (flake-input-url input))
                             (commit (tree-snapshot-commit snapshot))
                             (introduction
                              (match introduction
                                ((('commit commit) ('signer signer))
                                 (make-channel-introduction
                                  commit (openpgp-fingerprint signer))))))))
          (authenticate-channel chan (tree-snapshot-directory snapshot)
                                (tree-snapshot-commit snapshot))
          (values 'authenticated introduction))
        (values 'pinned-by-hash #f))))

(define (origin input)
  "Repository identity, independent of a particular checkout or revision."
  (if (eq? 'git (flake-input-type input))
      (cons 'git (flake-input-url input))
      (cons 'local (flake-input-path input))))

(define (resolve-input input path kind target)
  "Resolve INPUT without evaluating its outputs.  PATH is its dependency path."
  (let* ((snapshot
          (case (flake-input-type input)
            ((local)
             (if (flake-input-commit input)
                 (tracked-tree (flake-input-path input)
                               #:commit (flake-input-commit input)
                               #:worktree? (flake-input-worktree? input))
                 (tracked-tree (flake-input-path input)
                               #:worktree? (flake-input-worktree? input))))
            ((git) (resolve-git-input (flake-input-url input)
                                      #:branch (flake-input-branch input)
                                      #:tag (flake-input-tag input)
                                      #:commit (flake-input-commit input)))))
         (tree (verify-tree-snapshot snapshot)))
    (let-values (((status introduction) (authentication input snapshot)))
      (let ((input-name (flake-input-name input))
            (dependency-path path)
            (edge-kind kind)
            (edge-target target)
            (local-source (and (not (eq? 'replaced kind))
                               (flake-input-path input))))
        (locked-input
         (name input-name) (type (flake-input-type input))
         (url (flake-input-url input))
         ;; A committed local input retains its declared source for
         ;; verification.  Local overrides are in-memory only, never written.
         (path local-source)
         (branch (flake-input-branch input)) (tag (flake-input-tag input))
         (commit (tree-snapshot-commit snapshot))
         (tree-hash (tree-snapshot-hash snapshot))
         (as (flake-input-as input))
         (worktree? (flake-input-worktree? input))
         (authentication status) (introduction introduction)
         (checkout (tree-snapshot-directory snapshot))
         (options (flake-input-options input))
         (edges (if (null? dependency-path)
                    (list (lock-edge (from 'root) (name input-name)
                                     (kind edge-kind) (target edge-target)))
                    '())))))))

(define (input-at-path lock path)
  (if (null? path)
      #f
      (let ((name (car (reverse path))))
        (find (lambda (input)
                (and (eq? name (locked-input-name input))
                     (any (lambda (edge)
                            (eq? (if (null? (cdr path)) 'root
                                     (car (reverse (cdr path))))
                                 (lock-edge-from edge)))
                          (locked-input-edges input))))
              (flake-lock-inputs lock)))))

(define (nested-replacement input child)
  (or (assoc child (flake-input-overrides input))
      (and=> (assoc child (flake-input-follows input))
             (lambda (pair) (cons 'follows (cdr pair))))))

(define* (lock-flake directory #:key (update '()) (local-overrides '())
                     (write? #t))
  "Resolve the complete dependency graph without running any flake outputs."
  (let* ((directory (canonicalize-path directory))
         (flake (read-flake (string-append directory "/flake.scm")))
         (old (and (file-exists? (lock-file directory))
                   (read-flake-lock (lock-file directory))))
         (resolved '())
         (table '()))

    (define (previous-for name)
      (and old (find (lambda (candidate)
                       (eq? name (locked-input-name candidate)))
                     (flake-lock-inputs old))))

    (define (add-parent-edge! path name kind target)
      (unless (null? path)
        (let* ((parent-name (car (reverse path)))
               (parent-path (reverse (cdr (reverse path))))
               (parent (assoc (cons parent-path parent-name) table)))
          (when parent
            (let* ((old-parent (cdr parent))
                   (edge (lock-edge (from parent-name) (name name)
                                    (kind kind) (target target)))
                   (new-parent (locked-input (inherit old-parent)
                                (edges (cons edge (locked-input-edges old-parent))))))
              (set! table (acons (cons parent-path parent-name) new-parent table))
              (set! resolved (map (lambda (item)
                                    (if (eq? item old-parent) new-parent item))
                                  resolved)))))))

    (define (resolve-one input path kind target inherited-pinned?)
      (let* ((name (flake-input-name input))
             (key (cons path name))
             (existing (assoc key table)))
        (if existing
            (cdr existing)
            (let* ((override (and (null? path) (assoc name local-overrides)))
                   (effective (if override
                                  (sexp->input (cons name (cdr override))) input))
                   (kind (if override 'replaced kind))
                   (previous (previous-for name))
                   (reuse? (and previous (null? path) (not override)
                                (not (eq? update #t)) (not (memq name update))
                                (same-source? effective previous)
                                (not (eq? 'local (flake-input-type effective)))))
                   (locked (if reuse? previous
                               (resolve-input effective path kind target))))
              (set! table (acons key locked table))
              (set! resolved (append resolved (list locked)))
              (cond
               ((and (flake-input-pinned? effective)
                     (file-exists? (string-append (locked-input-checkout locked)
                                                  "/flake.lock")))
                (for-each
                 (lambda (child)
                   (let* ((child-name (locked-input-name child))
                          (child-path (append path (list name)))
                          (spec (append
                                 (case (locked-input-type child)
                                   ((local) `((local ,(locked-input-path child))))
                                   ((git) `((git ,(locked-input-url child)))))
                                 `((commit ,(locked-input-commit child))))))
                     (add-parent-edge! child-path child-name 'pinned #f)
                     (resolve-one (sexp->input (cons child-name spec))
                                  child-path 'pinned #f #t)))
                 (flake-lock-inputs
                  (read-flake-lock (string-append (locked-input-checkout locked)
                                                  "/flake.lock")))))
               ((and (eq? 'flake (flake-input-as effective))
                     (file-exists? (string-append
                                    (locked-input-checkout locked) "/flake.scm")))
                (let ((child-flake (read-flake
                                    (string-append (locked-input-checkout locked)
                                                   "/flake.scm"))))
                  (for-each
                   (lambda (child)
                     (let* ((child-name (flake-input-name child))
                            (replacement (nested-replacement effective child-name))
                            (root-input (find (lambda (candidate)
                                                (eq? child-name (flake-input-name candidate)))
                                              (flake-inputs flake)))
                            (same-origin (and root-input
                                              (equal? (origin child) (origin root-input))))
                            (pinned? (or inherited-pinned?
                                         (flake-input-pinned? effective)))
                            (follow? (and (not pinned?) same-origin))
                            (chosen (cond
                                     ((and replacement (pair? (cdr replacement))
                                           (eq? 'follows (car replacement)))
                                      root-input)
                                     (replacement
                                      (sexp->input (cons child-name replacement)))
                                     (follow? root-input)
                                     (else child)))
                            (child-path (append path (list name))))
                       (add-parent-edge! child-path child-name
                                         (cond (replacement 'replaced)
                                               (follow? 'followed)
                                               (pinned? 'pinned)
                                               (else 'direct))
                                         (and replacement (cdr replacement)))
                       (resolve-one chosen child-path
                                    (cond (replacement 'replaced)
                                          (follow? 'followed)
                                          (pinned? 'pinned)
                                          (else 'direct))
                                    (and replacement (cdr replacement)) pinned?)))
                   (flake-inputs child-flake)))))
              locked))))

    (for-each (lambda (input)
                (resolve-one input '()
                             (if (flake-input-pinned? input) 'pinned 'direct)
                             #f #f))
              (flake-inputs flake))
    (let ((result (flake-lock (version 1) (inputs resolved))))
      (when (and write? (null? local-overrides))
        (unless (and old (equal? (flake-lock->sexp old)
                                 (flake-lock->sexp result)))
          (write-flake-lock result (lock-file directory))))
      result)))

(define* (update-flake directory #:optional (input #t))
  (lock-flake directory #:update (if (string? input)
                                    (list (string->symbol input)) input)))

(define* (read-local-overrides directory)
  (let ((file (local-file directory)))
    (if (file-exists? file)
        (match (read-data-file file)
          (('flake-local ('version 1) ('inputs inputs ...))
           (map (lambda (input)
                  (cons (flake-input-name input)
                        (flake-input->local-spec input)))
                (map sexp->input inputs)))
          (_ (error "invalid flake.local.scm" file)))
        '())))

(define (flake-input->local-spec input)
  (append
   (case (flake-input-type input)
     ((local) `((local ,(flake-input-path input))))
     ((git) `((git ,(flake-input-url input)))))
   (if (flake-input-branch input) `((branch ,(flake-input-branch input))) '())
   (if (flake-input-tag input) `((tag ,(flake-input-tag input))) '())
   (if (flake-input-commit input) `((commit ,(flake-input-commit input))) '())
   (if (flake-input-worktree? input) '((worktree)) '())))

(define* (effective-flake-lock directory #:key (overrides '()))
  (let ((overrides (append overrides (read-local-overrides directory))))
    (if (null? overrides)
        (read-flake-lock (lock-file directory))
        (lock-flake directory #:local-overrides overrides #:write? #f))))

(define (verify-flake-lock directory)
  "Verify each locked checkout against its commit and content fingerprint."
  (let* ((flake (read-flake (string-append directory "/flake.scm")))
         (lock-data (read-flake-lock (lock-file directory))))
    (for-each
     (lambda (locked)
       (let* ((path (locked-input-path locked))
              (snapshot
               (case (locked-input-type locked)
                 ((local) (tracked-tree (or path
                                            (error "missing local input source"
                                                   (locked-input-name locked)))
                                       #:commit (locked-input-commit locked)
                                       #:worktree? (locked-input-worktree? locked)))
                 ((git) (resolve-git-input (locked-input-url locked)
                                           #:commit (locked-input-commit locked))))))
         (unless (equal? (locked-input-commit locked)
                         (tree-snapshot-commit snapshot))
           (error "checkout commit differs from lock" locked))
         (unless (equal? (locked-input-tree-hash locked)
                         (tree-snapshot-hash snapshot))
           (error "checkout content differs from lock" locked))
         (verify-tree-snapshot snapshot)))
     (flake-lock-inputs lock-data))
    lock-data))

(define (flake-metadata lock-data)
  (map (lambda (input)
         `((name . ,(locked-input-name input))
           (type . ,(locked-input-type input))
           (commit . ,(locked-input-commit input))
           (tree-hash . ,(locked-input-tree-hash input))
           (authentication . ,(locked-input-authentication input))
           (edges . ,(map (lambda (edge)
                           `((from . ,(lock-edge-from edge))
                             (name . ,(lock-edge-name edge))
                             (kind . ,(lock-edge-kind edge))
                             (target . ,(lock-edge-target edge))))
                         (locked-input-edges input)))))
       (flake-lock-inputs lock-data)))
