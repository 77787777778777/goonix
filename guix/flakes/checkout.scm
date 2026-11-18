;;; goonix: tracked Git trees and cached checkouts, with no store writes.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix flakes checkout)
  #:use-module (guix base32)
  #:use-module (guix build utils)
  #:use-module (guix git)
  #:use-module (guix hash)
  #:use-module (guix records)
  #:use-module ((guix config) #:select (%git))
  #:use-module (guix serialization)
  #:use-module (guix utils)
  #:use-module ((gcrypt hash) #:select (sha256))
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs io ports) #:select (get-bytevector-n put-bytevector))
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (tree-snapshot? tree-snapshot-directory tree-snapshot-commit
            tree-snapshot-hash tree-snapshot-dirty? tree-snapshot-source
            tracked-tree verify-tree-snapshot resolve-git-input))

(define-record-type* <tree-snapshot> tree-snapshot make-tree-snapshot
  tree-snapshot?
  (directory tree-snapshot-directory)
  (commit    tree-snapshot-commit)
  (hash      tree-snapshot-hash)
  (dirty?    tree-snapshot-dirty? (default #f))
  ;; The repository the snapshot was taken from, for local-change notices.
  ;; Runtime information; never serialized.
  (source    tree-snapshot-source (default #f)))

(define (git-output directory . arguments)
  "Run read-only git inspection in DIRECTORY; never execute repository hooks."
  (let* ((port (apply open-pipe* OPEN_READ %git
                      "-c" "core.hooksPath=/dev/null"
                      "-c" "protocol.file.allow=always"
                      "-C" directory arguments))
         (output (get-string-all port))
         (status (close-pipe port)))
    (unless (zero? status)
      (error "git inspection failed" directory arguments output))
    (string-trim-right output)))

(define (tracked-tree-hash file directory)
  "Hash FILE in DIRECTORY as a tracked file, preserving symlinks and exec bits."
  (let ((path (string-append directory "/" file)))
    (bytevector->nix-base32-string
     (file-hash* path #:recursive? #t
                 #:select? (const #t)))))

(define (tracked-content-hash directory files)
  "Hash the canonical list of tracked FILES.  The tree hash is versioned.
This is a locking fingerprint until the CA tree-hash format is available;
the lock records the format name separately in later versions."
  (bytevector->nix-base32-string
   (sha256
    (string->utf8
     (string-append
      "goonix-tracked-tree-v1\0"
      (string-join
       (map (lambda (file)
              (string-append file "\0" (tracked-tree-hash file directory) "\0"))
            (sort files string<?))
       ""))))))

(define (tree-files directory)
  (let walk ((root directory) (prefix ""))
    (append-map
     (lambda (name)
       (if (member name '("." "..")) '()
           (let* ((relative (if (string-null? prefix) name
                                (string-append prefix "/" name)))
                  (file (string-append root "/" name)))
             (if (eq? 'directory (stat:type (lstat file)))
                 (walk file relative)
                 (list relative)))))
     (scandir root))))

(define (export-tracked-tree directory commit worktree? destination)
  "Copy only Git-tracked content to DESTINATION, without project evaluation.
For WORKTREE? use the index plus working-tree modifications; otherwise export
COMMIT exactly.  git archive invokes no repository-defined code."
  (mkdir-p destination)
  (if worktree?
      (let ((files (string-split (git-output directory "ls-files" "-z" "--" ".") #\nul)))
        (for-each
         (lambda (file)
           (unless (string-null? file)
             (let ((source (string-append directory "/" file))
                   (target (string-append destination "/" file)))
               (mkdir-p (dirname target))
               (when (file-exists? source)
                 (if (eq? 'symlink (stat:type (lstat source)))
                     (symlink (readlink source) target)
                     (begin
                       (copy-file source target)
                       (chmod target (stat:mode (lstat source)))))))))
         files))
      (let ((archive (open-pipe* OPEN_READ %git
                                 "-c" "core.hooksPath=/dev/null"
                                 "-C" directory "archive" "--format=tar"
                                 commit)))
        ;; Consume the archive with tar; both processes receive fixed arguments.
        (let ((status (with-directory-excursion destination
                        (let ((input (open-pipe* OPEN_WRITE "tar" "-xf" "-")))
                          (let loop ((buffer (get-bytevector-n archive 65536)))
                            (unless (eof-object? buffer)
                              (put-bytevector input buffer)
                              (loop (get-bytevector-n archive 65536))))
                          (close-pipe input)))))
          (close-pipe archive)
          (unless (zero? status)
            (error "cannot export tracked tree" directory commit)))))
  destination)

(define* (tracked-tree directory #:key (worktree? #t) commit)
  "Snapshot DIRECTORY's tracked files and return their content fingerprint.
When COMMIT is given, export exactly that commit; otherwise use HEAD or the
tracked worktree according to WORKTREE?."
  (let* ((directory (canonicalize-path directory))
         (requested-commit commit)
         (commit (if requested-commit
                     (git-output directory "rev-parse"
                                 (string-append requested-commit "^{commit}"))
                     (git-output directory "rev-parse" "HEAD")))
         (dirty? (and worktree? (not requested-commit)
                      (not (string-null?
                            (git-output directory "status" "--porcelain" "--untracked-files=no"
                                        "--ignore-submodules=all" "--" ".")))))
         (temporary (mkdtemp "/tmp/goonix-tracked.XXXXXX")))
    (export-tracked-tree directory commit worktree? temporary)
    (tree-snapshot
     (directory temporary) (commit commit) (dirty? dirty?)
     (source directory)
     (hash (tracked-content-hash
            temporary
            (tree-files temporary))))))

(define (verify-tree-snapshot snapshot)
  "Recompute SNAPSHOT's tracked hash; refuse any modified tracked content."
  (let ((files (let walk ((root (tree-snapshot-directory snapshot))
                            (prefix ""))
                   (append-map
                    (lambda (name)
                      (if (member name '("." "..")) '()
                          (let* ((relative (if (string-null? prefix) name
                                               (string-append prefix "/" name)))
                                 (file (string-append root "/" name)))
                            (if (eq? 'directory (stat:type (lstat file)))
                                (walk file relative)
                                (list relative)))))
                    (scandir root)))))
      (let ((actual (tracked-content-hash (tree-snapshot-directory snapshot)
                                          (sort files string<?))))
        (unless (string=? actual (tree-snapshot-hash snapshot))
          (error "tampered tracked checkout"
                 (tree-snapshot-directory snapshot)
                 (tree-snapshot-hash snapshot) actual))
        #t)))

(define* (resolve-git-input url #:key branch tag commit worktree?
                            (repository-cache-directory
                             (%repository-cache-directory)))
  "Resolve URL to a cached checkout, then export the resolved commit to a
pristine tree.  A named COMMIT is always authoritative.  The snapshot never
points into the mutable cache: its content is exactly the locked commit."
  (let ((ref (cond (commit `(tag-or-commit . ,commit))
                   (tag `(tag . ,tag))
                   (branch `(branch . ,branch))
                   (else '()))))
    (let-values (((directory resolved relation)
                  ;; Each URL has its own directory below the cache root;
                  ;; passing the root itself would make all URLs share one
                  ;; repository.
                  (update-cached-checkout url #:ref ref
                                          #:cache-directory
                                          (url-cache-directory
                                           url repository-cache-directory))))
      (tracked-tree directory #:commit resolved #:worktree? #f))))
