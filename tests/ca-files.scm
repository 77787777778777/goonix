;;; goonix: CA-7 file-level sharing — file list, hash and assembly.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (test-ca-files)
  #:use-module (guix ca files)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;;; Commentary:
;;;
;;; The file-level substitution mechanism: a store item's files are listed
;;; with their NAR hashes (the hash .links keys on); a client assembles the
;;; tree fetching only files whose hash is not already in .links.  These tests
;;; exercise the list / hash / assembly logic on temporary trees, without a
;;; server.
;;;
;;; Code:

(test-begin "ca-files")

(define (make-tree root files)
  "Create FILES, an alist of (relative-path . content), under ROOT."
  (for-each (lambda (entry)
              (let ((path (car entry)) (content (cdr entry)))
                (let ((full (string-append root "/" path)))
                  (mkdir-p (dirname full))
                  (call-with-output-file full
                    (lambda (p) (display content p))))))
            files))

(define (read-all file) (call-with-input-file file get-string-all))

(test-group "file list and round trip"
  (call-with-temporary-directory
   (lambda (root)
     (make-tree root '(("a" . "alpha") ("sub/b" . "beta") ("sub/c" . "gamma")))
     (let* ((files (store-item-files root))
            (paths (map car files)))
       (test-equal "lists every file, sorted"
         '("a" "sub/b" "sub/c") paths)
       (test-assert "every entry has a nix-base32 hash and a size"
         (every (match-lambda
                  ((path type hash size)
                   (and (memq type '(regular executable symlink))
                        (= 52 (string-length hash))
                        (integer? size))))
                files))
       (test-equal "round trips through string"
         files
         (string->file-list (file-list->string files)))))))

(test-group "the hash is the .links NAR hash"
  (call-with-temporary-directory
   (lambda (root)
     (let ((file (string-append root "/f")))
       (call-with-output-file file (lambda (p) (display "content" p)))
       (chmod file #o444)
       (let ((entry (car (store-item-files root))))
         (test-equal "regular file hash matches file-hash* recursive"
           (bytevector->nix-base32-string (file-hash* file #:recursive? #t))
           (caddr entry))
         (test-eq "regular file type" 'regular (cadr entry)))
       (chmod file #o555)
       (let ((entry (car (store-item-files root))))
         (test-eq "executable file type" 'executable (cadr entry))
         (test-assert "execute bit changes the hash"
           (not (string=? (caddr entry)
                          (bytevector->nix-base32-string
                           (file-hash* file #:recursive? #f))))))))))

(test-group "assembly fetches only files not in .links"
  (call-with-temporary-directory
   (lambda (root)
     (let ((src (string-append root "/src"))
           (links (string-append root "/links"))
           (dst (string-append root "/dst")))
       (mkdir src) (mkdir links) (mkdir dst)
       (make-tree src '(("a" . "alpha") ("sub/b" . "beta") ("sub/c" . "gamma")))
       (let* ((files (store-item-files src))
              ;; Put "a" and "sub/b" into .links (hard links), leave "sub/c".
              (hash-a (caddr (assoc "a" files)))
              (hash-b (caddr (assoc "sub/b" files))))
         (link (string-append src "/a") (string-append links "/" hash-a))
         (link (string-append src "/sub/b") (string-append links "/" hash-b))
         (let* ((fetched-files '())
                (n (assemble-file-tree
                    files dst
                    (lambda (entry dest)
                      (set! fetched-files (cons (car entry) fetched-files))
                      ;; Fetch from src (the "server").
                      (copy-file (string-append src "/" (car entry)) dest))
                    #:links-dir links)))
           (test-equal "exactly one file fetched (the one not in .links)"
             1 n)
           (test-equal "the fetched file is sub/c"
             '("sub/c") fetched-files)
           (test-assert "the assembled tree has all three files"
             (and (string=? "alpha" (read-all (string-append dst "/a")))
                  (string=? "beta" (read-all (string-append dst "/sub/b")))
                  (string=? "gamma" (read-all (string-append dst "/sub/c")))))
           (test-assert "the linked files share the .links inode"
             (and (= (stat:ino (lstat (string-append dst "/a")))
                     (stat:ino (lstat (string-append links "/" hash-a))))
                  (= (stat:ino (lstat (string-append dst "/sub/b")))
                     (stat:ino (lstat (string-append links "/" hash-b))))))))))))

(test-group "a one-file change fetches one file on re-assembly"
  (call-with-temporary-directory
   (lambda (root)
     (let ((v1 (string-append root "/v1"))
           (v2 (string-append root "/v2"))
           (links (string-append root "/links"))
           (dst1 (string-append root "/dst1"))
           (dst2 (string-append root "/dst2")))
       (for-each mkdir (list v1 v2 links dst1 dst2))
       (make-tree v1 '(("a" . "alpha") ("b" . "beta") ("c" . "gamma")))
       ;; V2 changes only "b".
       (make-tree v2 '(("a" . "alpha") ("b" . "BETA-CHANGED") ("c" . "gamma")))
       (let* ((files1 (store-item-files v1))
              (files2 (store-item-files v2))
              ;; First assembly: nothing in .links, all three fetched.
              (n1 (assemble-file-tree
                   files1 dst1
                   (lambda (entry dest)
                     (copy-file (string-append v1 "/" (car entry)) dest))
                   #:links-dir links)))
         (test-equal "first assembly fetches all three" 3 n1)
         ;; Populate .links from dst1 (as the daemon's dedup would).
         (for-each (match-lambda
                     ((path type hash size)
                      (let ((lp (string-append links "/" hash)))
                        (unless (file-exists? lp)
                          (link (string-append dst1 "/" path) lp)))))
                   files1)
         ;; Second assembly of V2: only "b" changed, so only "b" is fetched.
         (let* ((fetched '())
                (n2 (assemble-file-tree
                     files2 dst2
                     (lambda (entry dest)
                       (set! fetched (cons (car entry) fetched))
                       (copy-file (string-append v2 "/" (car entry)) dest))
                     #:links-dir links)))
           (test-equal "re-assembly after a one-file change fetches one file"
             1 n2)
           (test-equal "the fetched file is b" '("b") fetched)
           (test-assert "the changed content is present"
             (string=? "BETA-CHANGED" (read-all (string-append dst2 "/b"))))))))))

(test-group "safe-item-file refuses paths that escape the item"
  (call-with-temporary-directory
   (lambda (root)
     (let ((item (string-append root "/item"))
           (outside (string-append root "/outside")))
       (mkdir item)
       (mkdir outside)
       (call-with-output-file (string-append outside "/secret")
         (lambda (p) (display "OUTSIDE-SECRET" p)))
       (make-tree item '(("a" . "alpha") ("sub/b" . "beta")))
       ;; A mid-path symlink leading out of the item.
       (symlink outside (string-append item "/lib"))
       (test-assert "a normal file resolves"
         (string=? (string-append item "/a")
                   (safe-item-file item "a")))
       (test-assert "a nested file resolves"
         (string=? (string-append item "/sub/b")
                   (safe-item-file item "sub/b")))
       (test-assert ".. is refused"
         (not (safe-item-file item "../outside/secret")))
       (test-assert "a mid-path .. is refused"
         (not (safe-item-file item "sub/../../outside/secret")))
       (test-assert "a leading . is refused"
         (not (safe-item-file item "./a")))
       (test-assert "an empty component is refused"
         (not (safe-item-file item "sub//b")))
       (test-assert "a mid-path symlink is refused"
         (not (safe-item-file item "lib/secret")))
       (test-assert "a missing file is refused"
         (not (safe-item-file item "nope")))
       ;; A store item that IS a symlink to a directory holding a secret:
       ;; walking through it would serve the secret, so it must be refused.
       (let ((symlink-item (string-append root "/symlink-item")))
         (symlink outside symlink-item)
         (test-assert "a symlink item is refused (no walking through it)"
           (not (safe-item-file symlink-item "secret")))
         (test-assert "store-item-files lists a symlink item as itself"
           (let ((entries (store-item-files symlink-item)))
             (and (= 1 (length entries))
                  (eq? 'symlink (cadr (car entries)))
                  (string=? "" (car (car entries)))))))))))

(test-end "ca-files")