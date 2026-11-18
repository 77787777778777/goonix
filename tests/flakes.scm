;;; goonix: tests for the flake data and lock formats.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (test-flakes)
  #:use-module (guix flakes)
  #:use-module (guix flakes checkout)
  #:use-module (guix tests git)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (git)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(test-begin "flakes")

(define (invoke-git directory . arguments)
  (let* ((pipe (apply open-pipe* OPEN_READ "git" "-C" directory arguments))
         (output (string-trim-right (get-string-all pipe)))
         (status (close-pipe pipe)))
    (unless (zero? status)
      (error "git command failed" arguments))
    output))

(define (first-commit directory)
  (invoke-git directory "rev-list" "--max-parents=0" "HEAD"))

(define (write-manifest directory specification)
  (call-with-output-file (string-append directory "/flake.scm")
    (lambda (port) (display specification port))))

(define (locked-by-name lock name)
  (find (lambda (input) (eq? name (locked-input-name input)))
        (flake-lock-inputs lock)))

(test-assert "flake.scm is read as data without running outputs"
  (let* ((directory (mkdtemp "/tmp/goonix-flake-data.XXXXXX"))
         (file (string-append directory "/flake.scm")))
    (call-with-output-file file
      (lambda (port)
        (display "(flake (format 1) (description \"safe\")\n\
 (modules \"guix\")\n\
 (inputs (dep (git \"https://example.org/dep\") (branch \"main\")\n\
               (as source)))\n\
 (options (debug? boolean #f \"Debug output\"))\n\
 (outputs (lambda (in) (error \"locking must not evaluate outputs\"))))\n"
                 port)))
    (let ((flake (read-flake file)))
      (and (= 1 (flake-format flake))
           (string=? "safe" (flake-description flake))
           (string=? "guix" (flake-modules flake))
           (equal? '(dep) (map flake-input-name (flake-inputs flake)))
           (equal? 'git (flake-input-type (first (flake-inputs flake))))
           (equal? 'source (flake-input-as (first (flake-inputs flake))))
           (equal? '(debug?) (map flake-option-name (flake-options flake)))
           ;; The output expression is retained, not evaluated.
           (pair? (flake-outputs-expression flake))))))

(test-assert "a reader extension in flake.scm is refused"
  (let* ((directory (mkdtemp "/tmp/goonix-flake-read.XXXXXX"))
         (file (string-append directory "/flake.scm")))
    (call-with-output-file file
      (lambda (port)
        (display "(flake (format 1) (description #.(begin (error \"executed\") \"x\")))"
                 port)))
    (guard (condition (#t #t))
      (read-flake file)
      #f)))

(test-assert "flake.lock round-trips as validated data"
  (let* ((directory (mkdtemp "/tmp/goonix-lock-data.XXXXXX"))
         (file (string-append directory "/flake.lock")))
    (call-with-output-file file
      (lambda (port)
        (write '(flake-lock
                 (version 1)
                 (inputs
                  ((dep (type git) (url "https://example.org/dep")
                        (branch "main")
                        (commit "0123456789012345678901234567890123456789")
                        (tree-hash "0123456789abcdfghijklmnpqrsvwxyz0123456789abcdfghi")
                        (authentication pinned-by-hash)
                        (edges ((edge (from dep) (name child)
                                      (kind followed) (target root))))))))
               port)))
    (let ((lock-data (read-flake-lock file)))
      (and (= 1 (flake-lock-version lock-data))
           (equal? 'dep (locked-input-name (first (flake-lock-inputs lock-data))))
           (equal? 'pinned-by-hash
                   (locked-input-authentication
                    (first (flake-lock-inputs lock-data))))
           (equal? 'followed
                   (lock-edge-kind
                    (first (locked-input-edges
                            (first (flake-lock-inputs lock-data))))))
           (begin
             (write-flake-lock lock-data file)
             (equal? lock-data (read-flake-lock file)))))))

(test-assert "a malformed lock is rejected"
  (let* ((directory (mkdtemp "/tmp/goonix-lock-bad.XXXXXX"))
         (file (string-append directory "/flake.lock")))
    (call-with-output-file file
      (lambda (port) (display "(flake-lock (version 99) (inputs))" port)))
    (guard (condition (#t #t))
      (read-flake-lock file)
      #f)))

(test-assert "local worktree hashes tracked content only"
  (with-temporary-git-repository directory
      '((add "tracked.txt" "tracked")
        (commit "initial"))
    (call-with-output-file (string-append directory "/ignored.txt")
      (lambda (port) (display "ignored" port)))
    (let ((first (tracked-tree directory)))
      (call-with-output-file (string-append directory "/ignored.txt")
        (lambda (port) (display "changed" port)))
      (let ((second (tracked-tree directory)))
        (and (equal? (tree-snapshot-hash first) (tree-snapshot-hash second))
             (equal? (tree-snapshot-commit first) (tree-snapshot-commit second))
             (not (tree-snapshot-dirty? first))
             (begin
               (call-with-output-file (string-append directory "/tracked.txt")
                 (lambda (port) (display "changed" port)))
               (not (equal? (tree-snapshot-hash first)
                            (tree-snapshot-hash (tracked-tree directory))))))))))

(test-assert "committed local input ignores untracked and modified files"
  (with-temporary-git-repository directory
      '((add "tracked.txt" "tracked")
        (commit "initial"))
    (call-with-output-file (string-append directory "/tracked.txt")
      (lambda (port) (display "modified" port)))
    (call-with-output-file (string-append directory "/ignored.txt")
      (lambda (port) (display "untracked" port)))
    (let ((snapshot (tracked-tree directory #:worktree? #f)))
      (and (not (tree-snapshot-dirty? snapshot))
           (file-exists? (string-append (tree-snapshot-directory snapshot)
                                        "/tracked.txt"))
           (not (file-exists? (string-append (tree-snapshot-directory snapshot)
                                             "/ignored.txt")))))))

(test-assert "lock is idempotent and never evaluates outputs"
  (with-temporary-git-repository dependency
      '((add "tracked.txt" "dependency") (commit "initial"))
    (call-with-temporary-directory
     (lambda (root)
       (call-with-output-file (string-append root "/flake.scm")
         (lambda (port)
           (format port "(flake (format 1) (inputs (dep (local ~s))) (outputs (lambda (in) (error \"must not run\"))))" dependency)))
       (let ((lock (lock-flake root)))
         (and (= 1 (length (flake-lock-inputs lock)))
              (equal? (flake-lock->sexp lock)
                      (flake-lock->sexp (lock-flake root)))
              (equal? (flake-lock->sexp lock)
                      (flake-lock->sexp (read-flake-lock
                                         (string-append root "/flake.lock"))))))))))

(test-assert "tampering with a local snapshot is refused"
  (with-temporary-git-repository directory
      '((add "tracked.txt" "tracked") (commit "initial"))
    (let ((snapshot (tracked-tree directory)))
      (call-with-output-file (string-append (tree-snapshot-directory snapshot)
                                            "/tracked.txt")
        (lambda (port) (display "tampered" port)))
      (guard (condition (#t #t))
        (verify-tree-snapshot snapshot)
        #f))))

(test-assert "local overrides leave the committed lock byte-identical"
  (with-temporary-git-repository original
      '((add "value.txt" "original") (commit "original"))
    (with-temporary-git-repository replacement
        '((add "value.txt" "replacement") (commit "replacement"))
      (call-with-temporary-directory
       (lambda (root)
         (call-with-output-file (string-append root "/flake.scm")
           (lambda (port)
             (format port "(flake (format 1) (inputs (dep (local ~s))))" original)))
         (lock-flake root)
         (let* ((file (string-append root "/flake.lock"))
                (before (call-with-input-file file get-string-all))
                (local (lock-flake root #:local-overrides
                                   `((dep . ((local ,replacement)))))))
           (and (string=? before (call-with-input-file file get-string-all))
                (not (equal? (locked-input-tree-hash
                              (first (flake-lock-inputs local)))
                             (locked-input-tree-hash
                              (first (flake-lock-inputs
                                      (read-flake-lock file)))))))))))))

(test-assert "worktree snapshots include staged additions"
  (with-temporary-git-repository directory
      '((add "tracked.txt" "old") (commit "initial")
        (add "staged.txt" "new"))
    (let ((snapshot (tracked-tree directory)))
      (and (file-exists? (string-append (tree-snapshot-directory snapshot)
                                        "/staged.txt"))
           (tree-snapshot-dirty? snapshot)))))

(test-assert "committed local inputs ignore staged and unstaged changes"
  (with-temporary-git-repository directory
      '((add "tracked.txt" "committed") (commit "initial")
        (add "staged.txt" "staged"))
    (call-with-output-file (string-append directory "/tracked.txt")
      (lambda (port) (display "modified" port)))
    (let ((snapshot (tracked-tree directory #:worktree? #f)))
      (and (not (tree-snapshot-dirty? snapshot))
           (not (file-exists? (string-append
                               (tree-snapshot-directory snapshot)
                               "/staged.txt")))
           (string=? "committed"
                     (call-with-input-file
                         (string-append (tree-snapshot-directory snapshot)
                                        "/tracked.txt")
                       get-string-all))))))

(test-assert "committed local locks retain a reproducible input path"
  (with-temporary-git-repository dependency
      '((add "tracked.txt" "committed") (commit "initial"))
    (call-with-temporary-directory
     (lambda (root)
       (call-with-output-file (string-append root "/flake.scm")
         (lambda (port)
           (format port "(flake (format 1) (inputs (dep (local ~s))))"
                   dependency)))
       (let* ((lock-data (lock-flake root))
              (locked (first (flake-lock-inputs lock-data))))
         (call-with-output-file (string-append dependency "/tracked.txt")
           (lambda (port) (display "ignored" port)))
         (and (locked-input-path locked)
              (locked-input-path
               (first (flake-lock-inputs
                       (read-flake-lock (string-append root "/flake.lock")))))
              (verify-flake-lock root)))))))

(test-assert "transitive local inputs are locked with their reasons"
  (with-temporary-git-repository leaf
      '((add "leaf.txt" "leaf") (commit "leaf"))
    (with-temporary-git-repository dependency
        '((add "dependency.txt" "dependency") (commit "dependency"))
      (call-with-output-file (string-append dependency "/flake.scm")
        (lambda (port)
          (format port "(flake (format 1) (inputs (leaf (local ~s))))"
                  leaf)))
      (with-git-repository dependency
          `((add "flake.scm"
                 ,(format #f "(flake (format 1) (inputs (leaf (local ~s))))" leaf))
            (commit "flake"))
        (const #t))
      (call-with-temporary-directory
       (lambda (root)
         (call-with-output-file (string-append root "/flake.scm")
           (lambda (port)
             (format port "(flake (format 1) (inputs (dependency (local ~s))))"
                     dependency)))
         (let ((lock-data (lock-flake root)))
           (and (= 2 (length (flake-lock-inputs lock-data)))
                (equal? '(leaf dependency)
                        (map lock-edge-name
                             (locked-input-edges
                              (find (lambda (input)
                                      (eq? 'dependency (locked-input-name input)))
                                    (flake-lock-inputs lock-data))))))))))))

(test-assert "cached git checkouts cannot satisfy a different locked commit"
  (with-temporary-git-repository dependency
      '((add "value.txt" "old") (commit "old"))
    (let ((old (first-commit dependency)))
      (with-git-repository dependency
          '((add "value.txt" "new") (commit "new"))
        (guard (condition (#t #f))
          (let ((snapshot (resolve-git-input dependency #:commit old)))
            (and (string=? old (tree-snapshot-commit snapshot))
                 (string=? "old"
                           (call-with-input-file
                               (string-append (tree-snapshot-directory snapshot)
                                              "/value.txt")
                             get-string-all)))))))))

(test-assert "pinned dependencies retain their own transitive pins"
  (with-temporary-git-repository shared
      '((add "value.txt" "old") (commit "old"))
    (let ((old (first-commit shared)))
      (with-git-repository shared
          '((add "value.txt" "new") (commit "new"))
        (with-temporary-git-repository dependency
            '((add "dependency.txt" "dependency") (commit "dependency"))
          (write-manifest
           dependency
           (format #f "(flake (format 1) (inputs (shared (local ~s))))"
                   shared))
          (with-git-repository dependency
              `((add "flake.scm"
                     ,(format #f "(flake (format 1) (inputs (shared (local ~s))))"
                              shared))
                (commit "flake"))
            (const #t))
          (let ((old (locked-input-commit
                      (locked-by-name (lock-flake dependency) 'shared))))
          (with-git-repository dependency
              `((add "flake.lock"
                     ,(call-with-input-file (string-append dependency "/flake.lock")
                        get-string-all))
                (commit "lock"))
            (const #t))
          (with-git-repository shared '((add "value.txt" "newest")
                                        (commit "newest"))
            (call-with-temporary-directory
             (lambda (root)
               (write-manifest
                root
                (format #f "(flake (format 1) (inputs (shared (local ~s)) (dependency (local ~s) (pinned))))"
                        shared dependency))
               (let* ((lock-data (lock-flake root))
                      (inputs (flake-lock-inputs lock-data))
                      (transitive (find
                                   (lambda (input)
                                     (and (locked-input-path input)
                                          (string=? shared (locked-input-path input))
                                          (null? (locked-input-edges input))))
                                   inputs))
                      (root-shared (find
                                    (lambda (input)
                                      (and (not (eq? transitive input))
                                           (locked-input-path input)
                                           (string=? shared
                                                     (locked-input-path input))))
                                    inputs)))
                 (and (= 3 (length inputs))
                      transitive root-shared
                      (string=? old (locked-input-commit transitive))
                      (not (string=? old (locked-input-commit root-shared)))
                      (not (eq? transitive root-shared)))))))))))))

(test-end)
