;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 7778777 <7778777@7778777.online>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.

(define-module (test-ca)
  #:use-module (guix ca)
  #:use-module (guix base16)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (json parser)
  #:use-module ((gcrypt hash) #:select (sha256))
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors))

(test-begin "ca")

(define vectors
  (let ((file (or (search-path (or (and=> (getenv "srcdir") list)
                                   '("."))
                               "tests/ca-vectors.json")
                  "tests/ca-vectors.json")))
    (call-with-input-file file json->scm)))

(define (field object name)
  (or (assoc-ref object name)
      (error "missing CA vector field" name)))

(define (list-field object name)
  (let ((value (field object name)))
    (unless (vector? value)
      (error "CA vector field is not a JSON array" name))
    (vector->list value)))

(test-group "tokens and placeholders"
  (test-assert "self token is deterministic and reserved"
    (let ((token (self-token "out")))
      (and (= 32 (string-length token))
           (not (char=? #\e (string-ref token 0)))
           (string-every (cut string-index "eout" <>) token)
           (string=? token (self-token "out")))))
  (test-assert "self token depends on output name"
    (not (string=? (self-token "out") (self-token "lib"))))
  (test-assert "token paths have store shape"
    (token-store-path? (token-path "out" "hello-1.0")))
  (test-assert "placeholders have store shape"
    (placeholder-path? (placeholder (string->utf8 "drv bytes")
                                    "out" "hello-1.0")))
  (test-equal "token vector matches the shared vectors"
    (field (field vectors "tokens") "out")
    (self-token "out"))
  (test-equal "lib token vector matches the shared vectors"
    (field (field vectors "tokens") "lib")
    (self-token "lib"))
  (test-equal "placeholder vector matches the shared vectors"
    (field (field vectors "placeholder") "hash")
    (let ((vector (field vectors "placeholder")))
      (substring (basename
                  (placeholder
                   (string->utf8 (field vector "derivationBytes"))
                   (field vector "output") (field vector "name")))
                 0 32)))
  (test-assert "self tokens are not placeholders"
    (not (placeholder-path? (token-path "out" "hello-1.0"))))
  (test-equal "placeholder identity is verified"
    (list (string->utf8 "drv bytes") "out" "hello-1.0")
    (placeholder-identity (placeholder (string->utf8 "drv bytes")
                                       "out" "hello-1.0")
                          (string->utf8 "drv bytes") "out" "hello-1.0"))
  (test-assert "invalid token text is rejected"
    (parameterize ((%store-prefix "/gnu/store"))
      (guard (condition (else #t))
        (validate-token-path "/gnu/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello")
        #f)))
  (test-assert "unknown reserved token is rejected"
    (parameterize ((%store-prefix "/gnu/store"))
      (guard (condition (else #t))
        (validate-token-path "/gnu/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-hello")
        #f)))
  (test-assert "real paths do not pass reserved validation"
    (parameterize ((%store-prefix "/gnu/store"))
      (guard (condition (else #t))
        (validate-token-path "/gnu/store/00000000000000000000000000000000-hello")
        #f)))
  (test-assert "a known token at the active store prefix is accepted"
    (string=? (token-path "out" "hello-1.0")
              (validate-token-path (token-path "out" "hello-1.0"))))
  (test-assert "a known placeholder is rejected"
    (guard (condition (else #t))
      (validate-token-path (placeholder (string->utf8 "drv bytes")
                                        "out" "hello-1.0"))
      #f)))

(test-group "tree and content vectors"
  (let ((root (mkdtemp "/tmp/ca-vectors.XXXXXX")))
    (for-each
     (lambda (vector)
       (let ((name (field vector "name"))
             (kind (field vector "kind")))
         (let ((path (string-append root "/" name)))
           (match kind
             ("regular"
              (call-with-output-file path
                (lambda (port)
                  (put-bytevector port (u8-list->bytevector
                                        (list-field vector "bytes"))))
                #:binary #t)
              (chmod path (if (= 1 (field vector "executable")) #o555 #o444)))
             ("symlink" (symlink (field vector "target") path))
             ("directory"
              (mkdir path)
              (for-each
               (lambda (entry)
                 (let ((child (string-append path "/" (field entry "name"))))
                   (call-with-output-file child
                     (lambda (port)
                       (put-bytevector port
                                       (u8-list->bytevector
                                        (list-field entry "bytes"))))
                     #:binary #t)
                   (chmod child #o444)))
               (list-field vector "entries"))))
           (test-equal (string-append "shared tree vector: " name)
             (field vector "sha256")
             (bytevector->base16-string (tree-hash path))))))
     (list-field vectors "trees"))
    (let ((ordered (string-append root "/ordered")))
      (mkdir ordered)
      (for-each
       (lambda (pair)
         (call-with-output-file (string-append ordered "/" (car pair))
           (lambda (port) (display (cdr pair) port))))
       '(("z-last" . "z") ("a-first" . "a")))
      (symlink "m-target" (string-append ordered "/m-link"))
      (test-equal "shared directory entry ordering"
        (field (field vectors "orderedDirectory") "sha256")
        (bytevector->base16-string (tree-hash ordered))))
    (let* ((content (field vectors "contentPaths"))
           (trees (list-field vectors "trees"))
           (tree (base16-string->bytevector (field (car trees) "sha256"))))
      (test-equal "shared content path"
        (field content "withoutReferences")
        (parameterize ((%store-prefix "/gnu/store"))
          (content-addressed-path "out" "hello" tree '())))
      (test-equal "shared content path with sorted references"
        (field content "withReferences")
        (parameterize ((%store-prefix "/gnu/store"))
          (content-addressed-path "out" "hello" tree
                                  (reverse (list-field content "references")))))))
  (let* ((resolved (field vectors "resolvedDerivation"))
         (bytes (string->utf8 (field resolved "bytes")))
         (drv (call-with-input-file
                  (begin
                    (let ((file (string-append (mkdtemp "/tmp/ca-drv.XXXXXX")
                                               "/resolved.drv")))
                      (call-with-output-file file
                        (lambda (port) (display (field resolved "bytes") port)))
                      file))
                read-derivation)))
    (test-equal "shared canonical resolved derivation bytes"
      bytes (derivation->bytevector drv))
    (test-equal "shared resolved-addressed path"
      (field resolved "resolvedPath")
      (parameterize ((%store-prefix "/gnu/store"))
        (resolved-addressed-path "out" "hello" bytes)))
    (test-equal "shared resolved-addressed sibling path"
      (field resolved "resolvedLibPath")
      (parameterize ((%store-prefix "/gnu/store"))
        (resolved-addressed-path "lib" "hello-lib" bytes)))))

(test-group "build record canonical payload"
  (let* ((vector (field vectors "record"))
         (payload (canonical-build-record-payload
                   (field vector "resolvedHash") (field vector "derivation")
                   (field vector "output") (field vector "path")
                   (field vector "origin")
                   #:relocation-checked?
                   (= 1 (field vector "relocationChecked")))))
    (test-equal "record payload bytes match the shared vectors"
      (u8-list->bytevector (list-field vector "payload")) payload)
    (test-equal "record payload hash matches the shared vectors"
      (field vector "sha256")
      (bytevector->base16-string (sha256 payload))))
  (test-assert "record payload is field-framed, not concatenated"
    (not (equal? (canonical-build-record-payload "resolved" "derivation"
                                                 "ou" "tpath" "origin")
                 (canonical-build-record-payload "resolved" "derivation"
                                                 "out" "path" "origin"))))
  (test-assert "relocation flag changes the payload"
    (not (equal? (canonical-build-record-payload "r" "d" "out" "p" "o"
                                                 #:relocation-checked? #f)
                 (canonical-build-record-payload "r" "d" "out" "p" "o"
                                                 #:relocation-checked? #t))))
  (test-assert "every signed field changes the payload"
    (let ((base (canonical-build-record-payload "r" "d" "out" "p" "o")))
      (and (not (equal? base (canonical-build-record-payload "r2" "d" "out" "p" "o")))
           (not (equal? base (canonical-build-record-payload "r" "d2" "out" "p" "o")))
           (not (equal? base (canonical-build-record-payload "r" "d" "lib" "p" "o")))
           (not (equal? base (canonical-build-record-payload "r" "d" "out" "p2" "o")))
           (not (equal? base (canonical-build-record-payload "r" "d" "out" "p" "o2")))))))

(test-end)
