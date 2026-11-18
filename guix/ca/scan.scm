;;; goonix: read-only measurements of store self references.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 7778777 <7778777@7778777.online>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.

(define-module (guix ca scan)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module ((guix store) #:select (%store-prefix))
  #:autoload (zlib) (make-gzip-input-port)
  #:autoload (lzma) (make-xz-input-port)
  #:export (<scan-report>
            scan-report?
            scan-report-item
            scan-report-files
            scan-report-bytes
            scan-report-full
            scan-report-fragments
            scan-report-utf16
            scan-report-hidden
            scan-report-names
            scan-report-links
            scan-report-undecodable
            scan-report-unsupported
            scan-report->alist
            scan-store-item
            store-directory-items))

(define-record-type <scan-report>
  (make-scan-report item files bytes full fragments utf16 hidden names links
                    undecodable unsupported)
  scan-report?
  (item scan-report-item)
  (files scan-report-files)
  (bytes scan-report-bytes)
  (full scan-report-full)
  (fragments scan-report-fragments)
  (utf16 scan-report-utf16)
  (hidden scan-report-hidden)
  (names scan-report-names)
  (links scan-report-links)
  (undecodable scan-report-undecodable)
  (unsupported scan-report-unsupported))

(define (scan-report->alist report)
  "Return REPORT as an alist suitable for JSON serialization."
  (list (cons "item" (scan-report-item report))
        (cons "files" (scan-report-files report))
        (cons "bytes" (scan-report-bytes report))
        (cons "full" (scan-report-full report))
        (cons "frag" (scan-report-fragments report))
        (cons "utf16" (scan-report-utf16 report))
        (cons "hidden" (scan-report-hidden report))
        (cons "names" (scan-report-names report))
        (cons "links" (scan-report-links report))
        (cons "undecodable" (scan-report-undecodable report))
        (cons "unsupported" (scan-report-unsupported report))))

(define (substring-bytes bytes start length)
  (let ((result (make-bytevector length)))
    (bytevector-copy! bytes start result 0 length)
    result))

(define (utf16-le-bytes hash)
  "Return the 32 ASCII characters of HASH in UTF-16LE, without a BOM."
  (let ((result (make-bytevector 64)))
    (let loop ((index 0))
      (if (= index 32)
          result
          (begin
            (bytevector-u8-set! result (* index 2)
                                (bytevector-u8-ref hash index))
            (loop (+ index 1)))))))

(define (latin1 bytes)
  (bytevector->string bytes "ISO-8859-1"))

(define (append-bytes first second)
  "Return a bytevector holding FIRST followed by SECOND."
  (let* ((size (bytevector-length first))
         (result (make-bytevector (+ size (bytevector-length second)))))
    (bytevector-copy! first 0 result 0 size)
    (bytevector-copy! second 0 result size (bytevector-length second))
    result))

(define (add-counts totals counts)
  "Return the alist TOTALS with the numbers of the alist COUNTS added."
  (fold (lambda (entry totals)
          (let ((old (or (assq-ref totals (car entry)) 0)))
            (alist-cons (car entry) (+ old (cdr entry))
                        (alist-delete (car entry) totals eq?))))
        totals
        counts))

(define (occurrence-ends text needle)
  "Return the offset just past each occurrence of NEEDLE in TEXT."
  (let ((size (string-length needle)))
    (let loop ((start 0) (ends '()))
      (let ((at (string-contains text needle start)))
        (if at
            (loop (+ at 1) (cons (+ at size) ends))
            ends)))))

(define (whole-hash-at? text hash-text start)
  "Return true if HASH-TEXT lies at offset START of TEXT."
  (and (>= start 0)
       (<= (+ start 32) (string-length text))
       (string-prefix? hash-text text 0 32 start (+ start 32))))

;; An 8-character window of the hash is a fragment unless the whole hash
;; lies around it.  Deciding that takes up to this many bytes after the
;; window.
(define %right-context 24)

;; Bytes kept from one read to the next: the longest needle (the hash in
;; UTF-16LE), the right context, and the left context of a window.
(define %retained 127)

(define* (scan-bytes hash bytes
                     #:optional (from 0) (to (bytevector-length bytes)))
  "Count the self references in BYTES that end after offset FROM and not
after offset TO.  Return an alist with the keys full, fragments and utf16.
Every occurrence counts, also one that overlaps the one before."
  (define text (latin1 bytes))
  (define hash-text (latin1 hash))
  (define (owned needle)
    (filter (lambda (end) (and (> end from) (<= end to)))
            (occurrence-ends text needle)))
  (define (fragments offset)
    (count (lambda (end)
             (not (whole-hash-at? text hash-text (- end 8 offset))))
           (owned (substring hash-text offset (+ offset 8)))))
  (list (cons 'full (length (owned hash-text)))
        (cons 'fragments (apply + (map fragments (iota 25))))
        (cons 'utf16 (length (owned (latin1 (utf16-le-bytes hash)))))))

(define* (scan-port hash port limit #:key (chunk-size (* 128 1024)))
  "Read at most LIMIT bytes from PORT, CHUNK-SIZE at a time, and return what
'scan-bytes' returns for all of them at once.

Each round scans the retained bytes followed by the new ones.  A round that
is not the last one leaves the occurrences that end in its last
%RIGHT-CONTEXT bytes to the next round, which has the bytes after them;
DECIDED is the offset up to which occurrences are already counted."
  (let loop ((retained #vu8()) (decided 0) (remaining limit) (totals '()))
    (let* ((wanted (min chunk-size remaining))
           (chunk (if (zero? wanted)
                      (eof-object)
                      (get-bytevector-n port wanted)))
           (last? (eof-object? chunk))
           (bytes (if last? retained (append-bytes retained chunk)))
           (size (bytevector-length bytes))
           (to (if last? size (max decided (- size %right-context))))
           (totals (add-counts totals (scan-bytes hash bytes decided to))))
      (if last?
          totals
          (let ((keep (min size %retained)))
            (loop (substring-bytes bytes (- size keep) keep)
                  (- to (- size keep))
                  (- remaining (bytevector-length chunk))
                  totals))))))

(define (compressed-kind file)
  (cond ((string-suffix? ".gz" file) 'gzip)
        ((or (string-suffix? ".xz" file) (string-suffix? ".lzma" file)) 'xz)
        ((string-suffix? ".bz2" file) 'unsupported-bzip2)
        ((any (lambda (suffix) (string-suffix? suffix file))
              '(".zip" ".jar" ".whl" ".egg" ".war" ".apk" ".epub" ".odt"))
         'unsupported-zip)
        (else #f)))

(define (open-decoder file kind)
  "Open FILE's decoder after resolving its module; installation errors escape."
  (resolve-interface (case kind ((gzip) '(zlib)) ((xz) '(lzma))))
  (let ((port (open-file file "r0")))
    (case kind
      ((gzip) (make-gzip-input-port port))
      ((xz) (make-xz-input-port port #:close? #t)))))

(define (scan-decoded hash input limit)
  "Scan INPUT and report whether decompression exceeded LIMIT."
  (let* ((counts (scan-port hash input limit))
         (extra (get-bytevector-n input 1)))
    (append (list (cons 'hidden (or (assq-ref counts 'full) 0)))
            (if (eof-object? extra) '() '((unsupported . 1))))))

(define* (scan-compressed hash file #:optional (limit (* 64 1024 1024)))
  "Return hidden self references, or a decoder/limit coverage counter."
  (let ((kind (compressed-kind file)))
    (if (memq kind '(gzip xz))
        (let ((input (open-decoder file kind)))
          (dynamic-wind
            (const #f)
            (lambda ()
              (if (eq? kind 'gzip)
                  (catch 'zlib-error
                    (lambda () (scan-decoded hash input limit))
                    (lambda _ '((undecodable . 1))))
                  (guard (condition
                          (((module-ref (resolve-interface '(lzma))
                                        'lzma-error?) condition)
                           '((undecodable . 1))))
                    (scan-decoded hash input limit))))
            (lambda () (close-port input))))
        (if kind '((unsupported . 1)) '()))))

(define (store-directory-items directory)
  "Return sorted directory store items below DIRECTORY, without following links."
  (sort (filter-map
         (lambda (name)
           (let ((path (string-append directory "/" name)))
             (and (> (string-length name) 33)
                  (char=? #\- (string-ref name 32))
                  (eq? 'directory (stat:type (lstat path)))
                  name)))
         (scandir directory))
        string<?))

(define (file-count entry)
  "Normalize a regular file's UTF-16 occurrence count to a presence count."
  (if (and (eq? (car entry) 'utf16) (> (cdr entry) 0))
      (cons 'utf16 1)
      entry))

(define (scan-store-item item)
  "Read ITEM's tree and return its self-reference report.  ITEM is a store
item name or a full path; no file is modified and no symlink is followed."
  (let* ((path (if (string-prefix? "/" item)
                   item (string-append (%store-prefix) "/" item)))
         (name (basename path))
         (hash (string->utf8 (string-take name 32)))
         (counters (make-vector 10 0))
         (keys '(files bytes full fragments utf16 hidden names links
                 undecodable unsupported)))
    (define (add! key amount)
      (let ((index (list-index (lambda (candidate) (eq? key candidate)) keys)))
        (vector-set! counters index (+ amount (vector-ref counters index)))))
    (define (add-file-counts! counts)
      (for-each (lambda (entry)
                  (add! (car entry) (cdr entry)))
                (map file-count counts)))
    (define (visit path depth)
      (let ((stat (lstat path)))
        (when (and (> depth 0)
                   (string-contains (basename path) (utf8->string hash)))
          (add! 'names 1))
        (case (stat:type stat)
          ((regular)
           (unless (zero? (stat:size stat))
             (add! 'files 1))
           (add! 'bytes (stat:size stat))
           (let ((file-port (open-file path "r0")))
             (dynamic-wind
               (const #f)
               (lambda ()
                 (add-file-counts!
                  (scan-port hash file-port (stat:size stat))))
               (lambda () (close-port file-port))))
           (for-each (lambda (entry) (add! (car entry) (cdr entry)))
                     (scan-compressed hash path)))
          ((symlink)
           (when (string-contains (readlink path) (utf8->string hash))
             (add! 'links 1)))
          ((directory)
           (for-each (lambda (entry)
                       (visit (string-append path "/" entry) (+ depth 1)))
                     (sort (scandir path
                                    (lambda (entry)
                                      (not (member entry '("." "..")))))
                           string<?))))))
    (visit path 0)
    (apply make-scan-report name (vector->list counters))))
