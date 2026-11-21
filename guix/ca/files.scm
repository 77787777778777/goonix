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
  #:use-module ((guix base32) #:select (%nix-base32-charset
                                         bytevector->nix-base32-string))
  #:use-module (guix serialization)
  #:use-module (guix store)
  #:use-module (guix narinfo)
  #:use-module ((guix http-client)
                #:select (http-fetch http-get-error? network-error?))
  #:use-module (guix diagnostics)
  #:use-module ((guix store deduplication)
                #:select (deduplicate nar-sha256))
  #:use-module ((guix build utils) #:select (mkdir-p delete-file-recursively))
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:export (store-item-files
            file-list->string
            string->file-list
            valid-file-list?
            links-path
            file-in-links?
            assemble-file-tree
            render-file-list
            render-file
            safe-item-file
            substitute-files
            substitute-files/nar
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

(define (valid-file-entry? entry)
  "Whether ENTRY is a well-formed file-list entry: a non-empty path whose
components are non-empty and not \".\" or \"..\" (or the empty path for a
single-file item), a known type, a nix-base32 sha256 hash of exactly 52
characters, and a non-negative integer size.  A cache is untrusted: any entry
failing this can name a path outside the destination or a .links file outside
the store, so the whole list is refused."
  (match entry
    ((path type hash size)
     (and (string? path)
          (or (string-null? path)
              (every (lambda (c)
                       (and (not (string-null? c))
                            (not (string=? c "."))
                            (not (string=? c ".."))))
                     (string-split path #\/)))
          (memq type '(regular executable symlink directory))
          (string? hash)
          (= 52 (string-length hash))
          (string-every %nix-base32-charset hash)
          (integer? size)
          (>= size 0)))
    (_ #f)))

(define (valid-file-list? files)
  "Whether FILES is a list of well-formed entries (see valid-file-entry?)."
  (and (list? files)
       (every valid-file-entry? files)))

(define (string->file-list str)
  "Parse STR, a serialized file list, back to a list of entries.  A list with
any malformed entry is refused whole (returns #f), so the substituter falls
back to the NAR rather than trusting a malicious cache's paths or hashes."
  (let ((lines (string-split str #\newline)))
    (match lines
      ((version rest ...)
       (and (string=? version %file-list-version)
            (let ((entries
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
                    rest)))
              ;; Every non-empty line must parse to a valid entry; a line that
              ;; does not parse, or an entry that fails validation, refuses the
              ;; whole list.
              (and (= (length entries)
                      (count (lambda (l) (not (string-null? l))) rest))
                   (valid-file-list? entries)
                   entries))))
      (_ #f))))

(define (links-path store hash)
  "Return the .links path for HASH in STORE's store directory."
  (string-append (%store-prefix) "/.links/" hash))

(define (file-in-links? store hash)
  "Whether STORE's .links already has a file with the nix-base32 NAR hash HASH."
  (file-exists? (links-path store hash)))

(define* (assemble-file-tree files root fetch-file #:key (links-dir #f))
  "Assemble the file tree described by FILES under ROOT.  FETCH-FILE is called
as (FETCH-FILE ENTRY) and returns an input port with the file's content (or,
for a symlink, its target string); it is called only for files whose hash is
not already in LINKS-DIR (when given), which are hard-linked instead.  Returns
the number of files fetched.

FILES is untrusted (it comes from a cache).  Every entry is validated first
and the whole list refused if any is bad.  The tree is built without following
symlinks: each parent directory is created with mkdir and checked with lstat to
be a real directory inside ROOT, an entry whose parent is a symlink is refused,
files are created fresh with O_NOFOLLOW|O_EXCL, and a .links entry is linked
only when it is a regular file.  Nothing is written or chmodded outside ROOT."
  (define fetched 0)

  (define (links-file hash)
    ;; HASH is validated (52 nix-base32 chars), so this cannot escape .links.
    (string-append links-dir "/" hash))

  (define (have-links? hash)
    (and links-dir
         (let ((lf (links-file hash)))
           (and (file-exists? lf)
                (eq? 'regular (stat:type (lstat lf)))))))

  (define (link-from-links hash dest)
    ;; Hard-link from .links; fall back to a copy on a different filesystem.
    ;; The .links entry is already read-only; its mode is left alone.
    (catch 'system-error
      (lambda () (link (links-file hash) dest))
      (lambda args
        (if (= EXDEV (system-error-errno args))
            (copy-file (links-file hash) dest)
            (apply throw args)))))

  (define (ensure-parents path)
    ;; Create each parent directory of PATH (relative to ROOT) with mkdir and
    ;; verify with lstat that it is a real directory inside ROOT.  Refuse if
    ;; any component is a symlink or a non-directory.
    (let loop ((dir root) (components (string-split path #\/)))
      (match components
        ((or () (_)) #t)                    ;no parent to create for the leaf
        ((next . more)
         (let ((sub (string-append dir "/" next)))
           (if (file-exists? sub)
               (unless (eq? 'directory (stat:type (lstat sub)))
                 (error "parent is not a real directory" sub))
               (begin (mkdir sub) (chmod sub #o755)))
           (loop sub more))))))

  (define (copy-in input output)
    (let loop ()
      (let ((bv (get-bytevector-n input 65536)))
        (unless (eof-object? bv)
          (put-bytevector output bv)
          (loop)))))

  (define (write-regular entry dest type)
    ;; Create DEST fresh with O_NOFOLLOW|O_EXCL so a symlink at DEST is never
    ;; followed and an existing file is never clobbered.  FETCH-FILE returns an
    ;; input port with the content.  chmod the file we created, never a path
    ;; that could resolve outside ROOT.
    (let ((out (open dest (logior O_WRONLY O_CREAT O_EXCL O_NOFOLLOW) #o600))
          (in (fetch-file entry)))
      (dynamic-wind
        (const #t)
        (lambda () (copy-in in out))
        (lambda () (close-port out) (close-port in))))
    (set! fetched (+ fetched 1))
    (chmod dest (if (eq? type 'executable) #o555 #o444)))

  (define (assemble-entry entry)
    (match entry
      ((path type hash size)
       (let ((dest (if (string-null? path) root (string-append root "/" path))))
         (unless (string-null? path)
           (ensure-parents path))
         (case type
           ((directory)
            (unless (file-exists? dest)
              (mkdir dest)
              (chmod dest #o755)))
           ((symlink)
            ;; FETCH-FILE returns the target string; stored as-is, never
            ;; followed during assembly.
            (symlink (fetch-file entry) dest))
           ((regular executable)
            (if (have-links? hash)
                (link-from-links hash dest)
                (write-regular entry dest type))))))))

  ;; Validate the whole untrusted list before the first write.
  (unless (valid-file-list? files)
    (error "invalid file list" files))
  ;; The destination root itself may not exist yet (the substituter passes a
  ;; fresh <temp>/restored path); create it as a real directory.
  (unless (file-exists? root)
    (mkdir root)
    (chmod root #o755))
  ;; Deepest paths last so parents exist; sort by depth then name.
  (for-each assemble-entry
            (sort files
                  (lambda (a b)
                    (let ((da (string-count (car a) #\/))
                          (db (string-count (car b) #\/)))
                      (if (= da db)
                          (string<? (car a) (car b))
                          (< da db))))))
  fetched)

(define (render-file-list store request name)
  "Render the file list of the store item named NAME (a base name) as a
text/plain response, for file-level substitution.  Returns two values: the
response headers and the body string.  A missing item gives a 404."
  (let ((item (string-append (%store-prefix) "/" name)))
    (if (valid-path? store item)
        (values '((content-type . (text/plain (charset . "UTF-8"))))
                (file-list->string (store-item-files item)))
        (values (build-response #:code 404) "not found"))))

(define (safe-item-file item path)
  "Resolve PATH (relative, slash-separated) under the store item directory
ITEM, returning the absolute file name only when it is a regular file safely
inside ITEM, else #f.  ITEM itself must be a real directory: a store item can
be a symlink with any target (a builder may write (symlink \"/etc\" out)), and
walking through it would serve files outside the store.  Rejects empty, \".\"
and \"..\" components, and refuses a symlink anywhere along the path (a
mid-path symlink could lead out of the item, and upstream's NAR serving never
follows one).  Each component is checked with lstat so the final component's
type is not the only guard."
  (and (not (string-null? path))
       (file-exists? item)
       (eq? 'directory (stat:type (lstat item)))
       (let ((components (string-split path #\/)))
         (and (every (lambda (c)
                       (and (not (string-null? c))
                            (not (string=? c "."))
                            (not (string=? c ".."))))
                     components)
              (let loop ((dir item) (rest components))
                (match rest
                  (() #f)
                  ((last)
                   (let ((file (string-append dir "/" last)))
                     (and (file-exists? file)
                          (eq? 'regular (stat:type (lstat file)))
                          file)))
                  ((next . more)
                   (let ((sub (string-append dir "/" next)))
                     ;; The intermediate component must be a real directory,
                     ;; not a symlink (which could point outside the item).
                     (and (file-exists? sub)
                          (eq? 'directory (stat:type (lstat sub)))
                          (loop sub more))))))))))

(define (render-file store request name path)
  "Render the raw bytes of one file (PATH, relative) of the store item NAME.
Returns two values: the response headers (with x-raw-file for the web server)
and #f.  A missing item or file, or a path that escapes the item, gives a 404."
  (define (not-found)
    (values (build-response #:code 404) "not found"))
  (let* ((item (string-append (%store-prefix) "/" name))
         (file (and (valid-path? store item)
                    (safe-item-file item path))))
    ;; Second guard: the canonical (symlink-resolved) file must stay inside the
    ;; canonical item directory.  safe-item-file already refuses a symlink on
    ;; any component; this catches anything that slips through.
    (if (and file
             (let ((canon-item (canonicalize-path item))
                   (canon-file (canonicalize-path file)))
               (string-prefix? (string-append canon-item "/") canon-file)))
        (values `((content-type . (application/octet-stream
                                   (charset . "ISO-8859-1")))
                  (x-raw-file . ,file))
                #f)
        (not-found))))

(define* (substitute-files cache-url name root #:key (links-dir #f) timeout)
  "File-level substitution: fetch the file list of the store item NAME from
the cache at CACHE-URL, then assemble its tree under ROOT, fetching only the
files whose NAR hash is not already in LINKS-DIR (hard-linking the rest).
Returns the number of files fetched over the network."
  (define (url . components)
    (string->uri (string-append (if (string-suffix? "/" cache-url)
                                    cache-url
                                    (string-append cache-url "/"))
                                (string-join components "/"))))
  (define (fetch-text uri)
    (let-values (((port resp) (http-fetch uri #:timeout timeout)))
      (let ((text (get-string-all port)))
        (close-port port)
        text)))
  (let* ((list-text (fetch-text (url "goonix" "files" name)))
         (files (string->file-list list-text)))
    (unless files
      (error "invalid file list from cache" name))
    (assemble-file-tree
     files root
     (lambda (entry)
       (match entry
         ((path 'symlink hash size)
          (error "symlink file-level fetch not supported" path))
         ((path type hash size)
          ;; Return an input port with the file's content; assemble-file-tree
          ;; creates the destination safely and copies from it.
          (let-values (((port resp)
                        (http-fetch (url "goonix" "file" name path)
                                    #:timeout timeout)))
            port))))
     #:links-dir links-dir)))

(define* (substitute-files/nar narinfo destination
                               #:key (links-dir #f) timeout
                               (open-connection-for-uri #f))
  "File-level substitution for the daemon's substituter: fetch the file list
of NARINFO's item from its cache, assemble the tree under DESTINATION
(hard-linking files already in LINKS-DIR, fetching the rest over
/goonix/file), and return two values like 'download-nar': the expected NAR
hash from NARINFO and the actual NAR hash of the assembled tree.  The caller's
bytevector=? check of those two is what makes file-level fetching safe; it is
unchanged.  Returns (values #f #f) when the cache offers no file list, so the
caller falls back to the NAR."
  (define cache-url (narinfo-uri-base narinfo))
  (define name (basename (narinfo-path narinfo)))
  (define (url . components)
    (string->uri (string-append (if (string-suffix? "/" cache-url)
                                    cache-url
                                    (string-append cache-url "/"))
                                (string-join components "/"))))
  (define (fetch-text uri)
    (let-values (((port resp) (http-fetch uri #:timeout timeout)))
      (let ((text (get-string-all port)))
        (close-port port)
        text)))
  (guard (c ((or (http-get-error? c) (network-error? c))
             ;; No file list (or unreachable): fall back to the NAR.
             (values #f #f)))
    (let* ((list-text (fetch-text (url "goonix" "files" name)))
           (files (string->file-list list-text)))
      (if (not files)
          ;; A malformed (malicious) file list: fall back to the NAR.
          (values #f #f)
          (begin
            ;; Delete DESTINATION first, as download-nar does.
            (catch 'system-error
              (lambda () (delete-file-recursively destination))
              (const #f))
            (assemble-file-tree
             files destination
             (lambda (entry)
               (match entry
                 ((path 'symlink hash size)
                  (error "symlink file-level fetch not supported" path))
                 ((path type hash size)
                  ;; Return an input port with the file's content;
                  ;; assemble-file-tree creates the destination safely
                  ;; (O_NOFOLLOW|O_EXCL) and copies.
                  (let-values (((port resp)
                                (http-fetch (url "goonix" "file" name path)
                                            #:timeout timeout)))
                    port))))
             #:links-dir links-dir)
            ;; Deduplicate the assembled files into the store's .links, as the
            ;; NAR path's dump-file/deduplicate does, so a later item sharing a
            ;; file hard-links it instead of fetching it again.  Only regular
            ;; files past the dedup minimum are registered.
            (when links-dir
              (for-each (match-lambda
                          ((path type hash size)
                           (when (memq type '(regular executable))
                             (let ((file (if (string-null? path)
                                             destination
                                             (string-append destination "/" path))))
                               (deduplicate file (nar-sha256 file)
                                            #:store (dirname links-dir))))))
                        files))
            ;; The actual NAR hash of the assembled tree, computed the same way
            ;; the daemon's hashPath does (file-hash* recursive writes the NAR
            ;; form).
            (let-values (((algorithm expected)
                          (narinfo-hash-algorithm+value narinfo)))
              (values expected
                      (file-hash* destination #:algorithm algorithm
                                  #:recursive? #t
                                  #:select? (const #t)))))))))

;;; files.scm ends here