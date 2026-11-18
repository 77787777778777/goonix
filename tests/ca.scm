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
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors))

(test-begin "ca")

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
  (test-equal "token vector matches the daemon"
    "ttteoeueuuottuuotutetetooueeoutu"
    (self-token "out"))
  (test-equal "lib token vector matches the daemon"
    "teuttuuueeuttoototeeeutooeeeuout"
    (self-token "lib"))
  (test-equal "placeholder vector matches the daemon"
    "eutuotetteeuuottttouuuteeutttuut"
    (substring (basename (placeholder (string->utf8 "drv bytes")
                                     "out" "hello-1.0")) 0 32))
  (test-assert "self tokens are not placeholders"
    (not (placeholder-path? (token-path "out" "hello-1.0"))))
  (test-equal "placeholder identity is verified"
    (list (string->utf8 "drv bytes") "out" "hello-1.0")
    (placeholder-identity (placeholder (string->utf8 "drv bytes")
                                       "out" "hello-1.0")
                          (string->utf8 "drv bytes") "out" "hello-1.0"))
  (test-assert "invalid token text is rejected"
    (guard (condition (else #t))
      (validate-token-path "/gnu/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello")
      #f))
  (test-assert "unknown reserved token is rejected"
    (guard (condition (else #t))
      (validate-token-path "/gnu/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-hello")
      #f))
  (test-assert "real paths do not pass reserved validation"
    (guard (condition (else #t))
      (validate-token-path "/gnu/store/00000000000000000000000000000000-hello")
      #f)))

(test-group "tree and content vectors"
  (let ((root (mkdtemp "/tmp/ca-vectors.XXXXXX")))
    (call-with-output-file (string-append root "/empty") (lambda (port) #t))
    (chmod (string-append root "/empty") #o444)
    (test-equal "empty file tree hash"
      "e9af47ca501ef1fa917417af3a6b6943e227a92536e97037c23c2c673b45ddf2"
      (bytevector->base16-string (tree-hash (string-append root "/empty"))))
    (symlink "target" (string-append root "/link"))
    (test-equal "symlink tree hash"
      "5ef2f8b90d0ca186d4b49cc6bbf69533d7bfe13bdc86bd4e7a2ef354ffb6812c"
      (bytevector->base16-string (tree-hash (string-append root "/link"))))
    (mkdir (string-append root "/dir"))
    (call-with-output-file (string-append root "/dir/empty") (lambda (port) #t))
    (chmod (string-append root "/dir/empty") #o444)
    (test-equal "directory tree hash"
      "4d906c085e9d72c6732872daeafd12323257f240329e76da0e9550d1ec24a2dd"
      (bytevector->base16-string (tree-hash (string-append root "/dir"))))
    (test-equal "content path"
      "/gnu/store/kq0f52qvhdwr2a7a5ycvrra8b124sdw4-hello"
      (parameterize ((%store-prefix "/gnu/store"))
        (content-addressed-path
         "out" "hello"
         (base16-string->bytevector
          "e9af47ca501ef1fa917417af3a6b6943e227a92536e97037c23c2c673b45ddf2")
         '())))))

(test-group "build record canonical payload"
  ;; These literal vectors are shared with the daemon's C++ records-test; the
  ;; two implementations must agree byte-for-byte (plan CA-0, CA-5).
  (test-equal "record payload matches the daemon vector"
    "da6009a94a97ea3c50f7528283ec6cc5f0f8d35ff413e0f5b0b562fa561a5002"
    (bytevector->base16-string
     (build-record-content-hash "resolved" "derivation" "out" "path" "origin"
                                #:relocation-checked? #t)))
  (test-equal "record payload length matches the daemon vector"
    104
    (bytevector-length
     (canonical-build-record-payload "resolved" "derivation" "out" "path"
                                     "origin" #:relocation-checked? #t)))
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
