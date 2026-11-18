;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 7778777 <7778777@7778777.online>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.

(define-module (guix ca)
  #:use-module (gcrypt hash)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module ((guix store) #:select (%store-prefix))
  #:export (%ca-store-directory
            self-token
            token-path
            token-store-path?
            placeholder
            placeholder-path?
            placeholder-identity
            validate-token-path
            canonical-build-record-payload
            build-record-content-hash))

(define %ca-store-directory "/gnu/store")
(define %reserved-alphabet "eotu")

(define (reserved-hash digest self?)
  "Encode 62 digest bits, with a self-token discriminator in character zero."
  (when (< (bytevector-length digest) 8)
    (error "CA token digest is too short" digest))
  (let ((result (make-string 32)))
    (string-set! result 0
                 (if self?
                     (string-ref %reserved-alphabet
                                 (+ 1 (modulo (bytevector-u8-ref digest 0) 3)))
                     #\e))
    (let loop ((index 1))
      (if (= index 32)
          result
          (let* ((bit (+ 2 (* 2 (- index 1))))
                 (byte (bytevector-u8-ref digest (quotient bit 8)))
                 (shift (- 6 (modulo bit 8)))
                 (value (logand (ash byte (- shift)) 3)))
            (string-set! result index (string-ref %reserved-alphabet value))
            (loop (+ index 1)))))))

(define (valid-name? name)
  (and (string? name)
       (not (zero? (string-length name)))
       (not (string-prefix? "." name))
       (not (string-contains name "/"))
       (string-every (lambda (character)
                       (or (char-alphabetic? character)
                           (char-numeric? character)
                           (string-index "+-._?=" character)))
                     name)))

(define (check-name name)
  (unless (valid-name? name)
    (error "invalid CA store name" name)))

(define (self-token output)
  (unless (and (string? output) (valid-name? output))
    (error "invalid output name" output))
  (reserved-hash (sha256 (string->utf8
                          (string-append "goonix:self-token:v1:" output)))
                 #t))

(define (token-path output name)
  (check-name name)
  (string-append (%store-prefix) "/" (self-token output) "-" name))

(define (placeholder derivation-bytes output name)
  (check-name name)
  (unless (and (bytevector? derivation-bytes)
               (string? output) (valid-name? output))
    (error "invalid placeholder input" derivation-bytes output))
  (let-values (((port get-hash) (open-hash-port (hash-algorithm sha256))))
    (put-bytevector port
                    (string->utf8
                     (string-append "goonix:placeholder:v1:"
                                    (number->string
                                     (bytevector-length derivation-bytes))
                                    ":" output ":")))
    (put-bytevector port derivation-bytes)
    (force-output port)
    (let ((hash (get-hash)))
      (close-port port)
      (string-append (%store-prefix) "/"
                     (reserved-hash hash #f) "-" name))))

(define (parse-reserved-path path)
  (and (string? path)
       (string-prefix? (string-append (%store-prefix) "/") path)
       (let ((base (basename path)))
         (and (> (string-length base) 33)
              (char=? #\- (string-ref base 32))
              (string-every (lambda (character)
                              (string-index %reserved-alphabet character))
                            (string-take base 32))
              (valid-name? (string-drop base 33))
              (list (string-take base 32) (string-drop base 33))))))

(define (token-store-path? path)
  (and=> (parse-reserved-path path)
         (lambda (parts)
           (not (char=? #\e (string-ref (car parts) 0))))))

(define (placeholder-path? path)
  (and=> (parse-reserved-path path)
         (lambda (parts)
           (char=? #\e (string-ref (car parts) 0)))))

(define (placeholder-identity path derivation-bytes output name)
  "Return the identity of PATH after proving it matches its declared inputs."
  (if (string=? path (placeholder derivation-bytes output name))
      (list derivation-bytes output name)
      (error "placeholder does not match the declared derivation and output"
             path)))

(define (validate-token-path path)
  (cond ((token-store-path? path) path)
        ((placeholder-path? path)
         (error "unresolved CA placeholder" path))
        (else (error "not a valid CA token or placeholder" path))))

;;;
;;; Build records: the Scheme mirror of the daemon's canonical signed payload.
;;; Format 1 (doc/goonix/content-addressing.md): domain string, version, then
;;; each field as a 64-bit little-endian length followed by its UTF-8 bytes,
;;; then the relocation-checked flag as a 64-bit integer.
;;;

(define (put-u64-le port value)
  (let ((bytes (make-bytevector 8 0)))
    (bytevector-u64-set! bytes 0 value (endianness little))
    (put-bytevector port bytes)))

(define (put-length-prefixed port text)
  (let ((bytes (string->utf8 text)))
    (put-u64-le port (bytevector-length bytes))
    (put-bytevector port bytes)))

(define* (canonical-build-record-payload resolved-hash derivation output
                                         path origin
                                         #:key (relocation-checked? #f))
  "Return the canonical bytes signed for a goonix build record.  This must
agree byte-for-byte with the daemon's canonicalBuildRecordPayload."
  (let-values (((port get-content) (open-bytevector-output-port)))
    (put-bytevector port (string->utf8 "goonix-record-v1"))
    (put-u8 port 0)
    (put-u64-le port 1)
    (for-each (lambda (field) (put-length-prefixed port field))
              (list resolved-hash derivation output path origin))
    (put-u64-le port (if relocation-checked? 1 0))
    (let ((bytes (get-content)))
      (close-port port)
      bytes)))

(define* (build-record-content-hash resolved-hash derivation output
                                    path origin
                                    #:key (relocation-checked? #f))
  "Return the SHA-256 (as a bytevector) of the canonical record payload."
  (sha256 (canonical-build-record-payload resolved-hash derivation output
                                          path origin
                                          #:relocation-checked?
                                          relocation-checked?)))
