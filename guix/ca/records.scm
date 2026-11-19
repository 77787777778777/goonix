;;; goonix: transport of signed content-addressing build records.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix ca records)
  #:use-module (guix base16)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (build-graph-records
            graph-response
            build-records->expression
            expression->build-records
            expression->graph-choices))

;;; Commentary:
;;;
;;; The HTTP representation is data, not a signature format: (goonix-records
;;; 1 (resolved-hash resolved-drv output path origin relocation signature)
;;; ...).  Resolved hashes are hexadecimal.  Every other field is a string.
;;; The receiving daemon authenticates signatures against its own ACL and
;;; recomputes R before selecting anything.

(define (build-graph-records store derivation)
  "Return the signed records of DERIVATION's build graph from STORE."
  (let loop ((fields (call-with-values
                         (lambda () (query-build-graph store derivation))
                       (lambda (records hints) records))) (records '()))
    (match fields
      (() (reverse records))
      ((hash derivation output path origin checked signature . rest)
       (loop rest (cons (list hash derivation output path origin checked signature)
                        records)))
      (_ (error "daemon returned incomplete goonix graph record" fields)))))

(define (graph-response store derivation)
  "Return DERIVATION's graph records and its original/output choice hints."
  (call-with-values (lambda () (query-build-graph store derivation))
    (lambda (fields hints)
      (let loop ((fields fields) (records '()))
        (match fields
          (() (values (reverse records) hints))
          ((hash derivation output path origin checked signature . rest)
           (loop rest (cons (list hash derivation output path origin checked
                                  signature)
                            records)))
          (_ (error "daemon returned incomplete goonix graph record" fields)))))))

(define (build-records->expression records)
  "Return the versioned, portable S-expression for RECORDS."
  `(goonix-records 1 ,@records))

(define (record-field? value)
  (and (string? value) (not (string-index value #\nul))))

(define (expression->build-records expression)
  "Validate and return the record lists in EXPRESSION."
  (match expression
    (('goonix-records 1 records ... ('choices (choices ...)))
     (expression->build-records `(goonix-records 1 ,@records)))
    (('goonix-records 1 records ...)
     (for-each
      (match-lambda
        ((hash derivation output path origin checked signature)
         (unless (and (record-field? derivation)
                      (record-field? output)
                      (record-field? path)
                      (record-field? origin)
                      (record-field? signature)
                      (member checked '("0" "1"))
                      (string? hash)
                      (= 64 (string-length hash))
                      (string-every (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6
                                              #\7 #\8 #\9 #\a #\b #\c #\d
                                              #\e #\f) hash))
           (error "invalid goonix build record" expression)))
        (_ (error "malformed goonix build record" expression)))
      records)
     records)
    (_ (error "unsupported goonix record graph" expression))))

(define (expression->graph-choices expression)
  "Validate and return the unsigned original/output/path choice hints."
  (match expression
    (('goonix-records 1 records ... ('choices (choices ...)))
     (for-each
      (match-lambda
        ((derivation output path)
         (unless (and (record-field? derivation)
                      (record-field? output)
                      (record-field? path))
           (error "invalid goonix graph choice hint" expression)))
        (_ (error "malformed goonix graph choice hint" expression)))
      choices)
     (append-map identity choices))
    (_ (error "missing goonix graph choice hints" expression))))
