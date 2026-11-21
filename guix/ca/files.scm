;;; goonix: file-level sharing — list, hash and assemble a store item's files.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix ca files)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix serialization)
  #:use-module (guix store)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (store-item-files
            file-list->string
            string->file-list
            links-path
            file-in-links?
            assemble-file-tree
            %file-list-version))

;;; Commentary:
;;;
;;; File-level substitution (plan section 9.1): the server lists the file
;;; hashes of an item; the client fetches only files it does not already have
;;; in /gnu/store/.links.  The hash of a file is the NAR serialisation hash
;;; (hashPath in nix/libstore/optimise-store.cc), which includes the execute
;;; bit; file-hash* with #:recursive? #t computes the same hash, and the
;;; .links entry is .links/<nix-base32-hash>.
;;;
;;; A file list is a list of entries (RELATIVE-PATH TYPE HASH SIZE), where
;;; RELATIVE-PATH is relative to the item root ("" for a single-file item),
;;; TYPE is regular, executable or symlink, HASH is the nix-base32 NAR hash
;;; (for a symlink, the hash of its target string's NAR form), and SIZE is the
;;; byte size (for a symlink, the target string length).  Directories are
;;; implied by the paths and are not listed.
;;;
;;; Code:

(define %file-list-version "goonix-files-1")

(define (file-nar-hash file)
  "Return the nix-base32 NAR hash of FILE, the hash .links keys on."
  (bytevector->nix-base32-string (file-hash* file #:recursive? #t)))

(define (symlink-nar-hash target)
  "Return the nix-base32 NAR hash of a symlink whose target is TARGET."
  ;; The NAR form of a symlink hashes its target string; write-file on a
  ;; temporary symlink gives the same hash the daemon's hashPath computes.
  (let* ((dir (mkdtemp "/tmp/goonix-symlink-XXXXXX"))
         (link (string-append dir "/l")))
    (symlink target link)
    (let ((h (file-hash* link #:recursive? #t)))
      (delete-file link)
      (rmdir dir)
      (bytevector->nix-base32-string h))))

(define (store-item-files root)
  "Return the file list of the store item at ROOT: a list of (RELATIVE-PATH
TYPE HASH SIZE) entries, sorted by path.  ROOT may be a directory or a single
regular file / symlink."
  (define (walk file relative)
    (let ((stat (lstat file)))
      (case (stat:type stat)
        ((directory)
         (append-map
          (lambda (entry)
            (walk (string-append file "/" entry)
                  (if (string-null? relative)
                      entry
                      (string-append relative "/" entry))))
          (sort (scandir file (lambda (e) (not (member e '("." "..")))))
                string<?)))
        ((regular)
         (let ((exec? (logand (stat:mode stat) #o111)))
           (list (list relative
                       (if (zero? exec?) 'regular 'executable)
                       (file-nar-hash file)
                       (stat:size stat)))))
        ((symlink)
         (let ((target (readlink file)))
           (list (list relative 'symlink
                       (symlink-nar-hash target)
                       (string-length target)))))
        (else '()))))
  (walk root ""))

(define (file-list->string files)
  "Serialize FILES, a file list, to a string (one entry per line)."
  (call-with-output-string
    (lambda (port)
      (format port "~a~%" %file-list-version)
      (for-each (match-lambda
                  ((path type hash size)
                   (format port "~a ~a ~a ~a~%"
                           (if (string-null? path) "." path)
                           type hash size)))
                files))))

(define (string->file-list str)
  "Parse STR, a serialized file list, back to a list of entries."
  (let ((lines (string-split str #\newline)))
    (match lines
      ((version rest ...)
       (unless (string=? version %file-list-version)
         (error "unknown file list version" version))
       (filter-map
        (lambda (line)
          (and (not (string-null? line))
               (match (string-tokenize line)
                 ((path type hash size)
                  (list (if (string=? path ".") "" path)
                        (string->symbol type)
                        hash
                        (string->number size)))
                 (_ #f))))
        rest)))))

(define (links-path store hash)
  "Return the .links path for HASH in STORE's store directory."
  (string-append (%store-prefix) "/.links/" hash))

(define (file-in-links? store hash)
  "Whether STORE's .links already has a file with the nix-base32 NAR hash HASH."
  (file-exists? (links-path store hash)))

(define* (assemble-file-tree files root fetch-file #:key (links-dir #f))
  "Assemble the file tree described by FILES under ROOT.  FETCH-FILE is called
as (FETCH-FILE ENTRY DESTINATION) to write a file's content; it is called only
for files whose hash is not already in LINKS-DIR (when given), which are
hard-linked instead.  Returns the number of files fetched."
  (define fetched 0)
  (define (have-links? hash)
    (and links-dir (file-exists? (string-append links-dir "/" hash))))
  (define (link-from-links hash dest)
    (link (string-append links-dir "/" hash) dest))
  (for-each
   (match-lambda
     ((path type hash size)
      (let ((dest (if (string-null? path) root (string-append root "/" path))))
        (unless (file-exists? (dirname dest))
          (mkdir-p (dirname dest)))
        (case type
          ((symlink)
           ;; Symlinks are cheap; always recreate (not fetched).
           (symlink (fetch-file (list path type hash size) 'symlink-target)
                    dest))
          ((regular executable)
           (cond
            ((have-links? hash)
             (link-from-links hash dest))
            (else
             (fetch-file (list path type hash size) dest)
             (set! fetched (+ fetched 1))))
           (chmod dest (if (eq? type 'executable) #o555 #o444)))))))
   ;; Deepest paths last so parents exist; sort by depth then name.
   (sort files (lambda (a b)
                 (let ((da (string-count (car a) #\/))
                       (db (string-count (car b) #\/)))
                   (if (= da db)
                       (string<? (car a) (car b))
                       (< da db))))))
  fetched)

;;; files.scm ends here