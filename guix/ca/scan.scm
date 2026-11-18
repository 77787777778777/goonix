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

(define (increment key result amount)
  (cons (cons key (+ amount (or (assoc-ref result key) 0)))
        (assoc-remove! result key)))

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

(define (count-after needle haystack pending-length)
  "Count NEEDLE in Latin-1 HAYSTACK, excluding the retained prefix."
  (let loop ((start 0) (count 0))
    (let ((at (string-contains haystack needle start)))
      (if (not at)
          count
          (loop (+ at 1)
                (if (>= (+ at (string-length needle)) pending-length)
                    (+ count 1) count))))))

(define* (scan-bytes hash bytes #:optional (pending-length 0))
  "Count self references using native searches over arbitrary binary bytes."
  (let* ((text (latin1 bytes))
         (hash-text (latin1 hash))
         (full (count-after hash-text text pending-length))
         (encoded (count-after (latin1 (utf16-le-bytes hash)) text
                               pending-length))
         (windows
          (let loop ((offset 0) (result '()))
            (if (> offset (- (string-length hash-text) 8))
                (reverse result)
                (loop (+ offset 1)
                      (cons (cons offset
                                  (substring hash-text offset (+ offset 8)))
                            result))))))
    (define (fragment-count window total)
      (let ((offset (car window))
            (needle (cdr window)))
        (let loop ((start 0) (count 0))
          (let ((at (string-contains text needle start)))
            (if (not at)
                (+ total count)
                (let ((whole (- at offset)))
                  (loop (+ at 1)
                        (+ count
                           (if (and (>= (+ at 8) pending-length)
                                    (or (< whole 0)
                                        (> (+ whole 32) (string-length text))
                                        (not (string=? hash-text
                                                       (substring text whole (+ whole 32))))))
                               1 0)))))))))
    (let ((fragments (fold fragment-count 0 windows)))
      (list (cons 'full full)
            (cons 'fragments fragments)
            (cons 'utf16 encoded)))))

(define (scan-port hash port limit)
  "Read PORT in bounded chunks, retaining HASH's largest encoded suffix."
  (let ((width 128)                       ;UTF-32LE: 32 characters
        (chunk-size (* 128 1024)))
    (let loop ((pending #vu8()) (result '()) (consumed 0))
      (let* ((chunk (get-bytevector-n port chunk-size))
             (eof? (eof-object? chunk))
             (chunk (if eof? #vu8() chunk))
             (limit? (>= (+ consumed (bytevector-length chunk)) limit))
             (chunk (if limit?
                        (substring-bytes chunk 0
                                         (max 0 (- limit consumed))) chunk)))
        (if (and (or eof? limit?) (zero? (bytevector-length chunk)))
            result
            (let* ((pending-length (bytevector-length pending))
                   (bytes (let ((combined (make-bytevector
                                           (+ pending-length
                                              (bytevector-length chunk)))))
                            (bytevector-copy! pending 0 combined 0 pending-length)
                            (bytevector-copy! chunk 0 combined pending-length
                                              (bytevector-length chunk))
                            combined))
                   (result (fold (lambda (entry result)
                                   (increment (car entry) result (cdr entry)))
                                 result (scan-bytes hash bytes pending-length))))
              (if (or eof? limit?)
                  result
                  (loop (substring-bytes bytes
                                         (max 0 (- (bytevector-length bytes)
                                                   (- width 1)))
                                         (min (- width 1)
                                              (bytevector-length bytes)))
                        result (+ consumed (bytevector-length chunk))))))))))

(define (compressed-kind file)
  (cond ((string-suffix? ".gz" file) 'gzip)
        ((or (string-suffix? ".xz" file) (string-suffix? ".lzma" file)) 'xz)
        ((string-suffix? ".bz2" file) 'unsupported-bzip2)
        ((any (lambda (suffix) (string-suffix? suffix file))
              '(".zip" ".jar" ".whl" ".egg" ".war" ".apk" ".epub" ".odt"))
         'unsupported-zip)
        (else #f)))

(define (scan-compressed hash file)
  "Return the hidden self-reference count or an unsupported/error counter."
  (let ((kind (compressed-kind file)))
    (cond ((not kind) '())
          ((eq? kind 'gzip)
           (catch #t
             (lambda ()
               (let* ((file-port (open-file file "r0"))
                      (input (make-gzip-input-port file-port)))
                 (dynamic-wind
                   (const #f)
                   (lambda ()
                     (let ((result (scan-port hash input (* 64 1024 1024))))
                       (list (cons 'hidden (or (assoc-ref result 'full) 0)))))
                   (lambda () (close-port input) (close-port file-port)))))
             (lambda _ (list (cons 'undecodable 1)))))
          ((eq? kind 'xz)
           (catch #t
             (lambda ()
               (let* ((file-port (open-file file "r0"))
                      (input (make-xz-input-port file-port)))
                 (dynamic-wind
                   (const #f)
                   (lambda ()
                     (let ((result (scan-port hash input (* 64 1024 1024))))
                       (list (cons 'hidden (or (assoc-ref result 'full) 0)))))
                   (lambda () (close-port input) (close-port file-port)))))
             (lambda _ (list (cons 'undecodable 1)))))
          (else (list (cons 'unsupported 1))))))

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
    (define (visit path depth)
      (let ((stat (lstat path)))
        (case (stat:type stat)
          ((regular)
           (add! 'files 1)
           (add! 'bytes (stat:size stat))
           (let ((file-port (open-file path "r0")))
             (dynamic-wind
               (const #f)
               (lambda ()
                 (for-each (lambda (entry) (add! (car entry) (cdr entry)))
                           (scan-port hash file-port (stat:size stat))))
               (lambda () (close-port file-port))))
           (for-each (lambda (entry) (add! (car entry) (cdr entry)))
                     (scan-compressed hash path)))
          ((symlink)
           (when (string-contains (readlink path) (utf8->string hash))
             (add! 'links 1)))
          ((directory)
           (when (and (> depth 0)
                      (string-contains (basename path) (utf8->string hash)))
             (add! 'names 1))
           (for-each (lambda (entry)
                       (visit (string-append path "/" entry) (+ depth 1)))
                     (sort (scandir path
                                    (lambda (entry)
                                      (not (member entry '("." "..")))))
                           string<?))))))
    (visit path 0)
    (apply make-scan-report name (vector->list counters))))
