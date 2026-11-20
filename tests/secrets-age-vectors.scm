;;; goonix: published C2SP/CCTV age vectors for the supported vault subset.
(use-modules (guix secrets age) (gcrypt base16) (gcrypt hash)
             (rnrs bytevectors) (ice-9 binary-ports) (ice-9 iconv)
             (srfi srfi-1) (srfi srfi-64) (zlib))

(define (fixture-root)
  (or (getenv "GOONIX_AGE_VECTORS") "tests/fixtures/age"))
(define (slice bv start)
  (let ((r (make-bytevector (- (bytevector-length bv) start))))
    (let loop ((i start))
      (when (< i (bytevector-length bv))
        (bytevector-u8-set! r (- i start) (bytevector-u8-ref bv i))
        (loop (+ i 1)))) r))
(define (metadata-line line)
  (let ((i (string-index line #\:)))
    (unless i (error "invalid age fixture metadata" line))
    (cons (substring line 0 i) (substring line (+ i 2)))))
(define (read-vector name)
  (let* ((bytes (call-with-input-file (string-append (fixture-root) "/" name)
                  get-bytevector-all))
         (lines (string-split (bytevector->string bytes "ISO-8859-1") #\newline))
         (meta (take-while (lambda (l) (not (string-null? l))) lines))
         (start (+ 1 (apply + (map (lambda (l) (+ 1 (string-length l))) meta))))
         (cipher (slice bytes start))
         (meta (map metadata-line meta)))
    (values meta
            (if (assoc-ref meta "compressed")
                (call-with-zlib-input-port (open-bytevector-input-port cipher)
                  get-bytevector-all)
                cipher))))

(test-begin "secrets-age-published-vectors")
(for-each
 (lambda (name)
   (call-with-values (lambda () (read-vector name))
     (lambda (meta cipher)
       (let ((expected (assoc-ref meta "expect"))
             (ids (map cdr (filter (lambda (x) (string=? (car x) "identity")) meta)))
             (passphrase (assoc-ref meta "passphrase")))
         (if (string=? expected "success")
             (test-equal (string-append name " decrypts and hashes correctly")
               (base16-string->bytevector (assoc-ref meta "payload"))
               (bytevector-hash
                (decrypt-age-file cipher
                                  (if passphrase `((scrypt . ,passphrase)) ids))
                (hash-algorithm sha256)))
             (test-error (string-append name " is refused: " expected) #t
               (decrypt-age-file cipher
                                 (if passphrase `((scrypt . ,passphrase)) ids))))))))
 (list "armor_crlf" "armor_full_last_line" "armor_no_eol" "armor_scrypt"
       "armor_whitespace_outside" "armor_x25519" "scrypt" "stanza_empty_body"
       "stanza_empty_last_line" "stanza_valid_characters" "stream_257_chunks"
       "stream_257_chunks_full" "stream_258_chunks" "stream_empty_payload"
       "stream_last_chunk_full" "stream_last_chunk_full_second"
       "stream_three_chunks" "stream_two_chunks" "x25519" "x25519_grease"
       "x25519_multiple_recipients"
       "armor_cr_between_padding" "armor_cr_in_body" "armor_cr_line_start"
       "armor_double_crlf" "armor_empty" "armor_empty_last_line"
       "armor_empty_line_begin" "armor_empty_line_end" "armor_eol_between_padding"
       "armor_garbage_encoded" "armor_garbage_leading" "armor_garbage_trailing"
       "armor_header_crlf" "armor_headers" "armor_invalid_character_header"
       "armor_invalid_character_payload" "armor_long_line" "armor_lowercase"
       "armor_no_end_line" "armor_no_match" "armor_no_padding"
       "armor_not_canonical" "armor_pgp_checksum" "armor_short_line"
       "armor_whitespace_begin" "armor_whitespace_end" "armor_whitespace_eol"
       "armor_whitespace_last_line" "armor_whitespace_line_start" "armor_wrong_type"
       "empty" "header_crlf" "hmac_bad" "hmac_extra_space" "hmac_garbage"
       "hmac_missing" "hmac_no_space" "hmac_not_canonical" "hmac_trailing_space"
       "hmac_truncated" "scrypt_and_x25519" "scrypt_bad_tag" "scrypt_double"
       "scrypt_extra_argument" "scrypt_long_file_key" "scrypt_no_match"
       "scrypt_not_canonical_body" "scrypt_not_canonical_salt" "scrypt_salt_long"
       "scrypt_salt_missing" "scrypt_salt_short" "scrypt_uppercase"
       "scrypt_work_factor_23" "scrypt_work_factor_hex"
       "scrypt_work_factor_leading_garbage" "scrypt_work_factor_leading_plus"
       "scrypt_work_factor_leading_zero_decimal" "scrypt_work_factor_leading_zero_octal"
       "scrypt_work_factor_missing" "scrypt_work_factor_negative"
       "scrypt_work_factor_overflow" "scrypt_work_factor_trailing_garbage"
       "scrypt_work_factor_wrong" "scrypt_work_factor_zero" "stanza_bad_start"
       "stanza_base64_padding" "stanza_empty_argument" "stanza_invalid_character"
       "stanza_long_line" "stanza_missing_body" "stanza_missing_final_line"
       "stanza_multiple_short_lines" "stanza_no_arguments" "stanza_not_canonical"
       "stanza_spurious_cr" "stream_bad_tag" "stream_bad_tag_second_chunk"
       "stream_bad_tag_second_chunk_full" "stream_last_chunk_empty"
       "stream_missing_tag" "stream_no_chunks" "stream_no_final"
       "stream_no_final_full" "stream_no_final_two_chunks"
       "stream_no_final_two_chunks_full" "stream_no_nonce" "stream_short_chunk"
       "stream_short_nonce" "stream_short_second_chunk" "stream_trailing_garbage_long"
       "stream_trailing_garbage_short" "stream_two_final_chunks"
       "stream_two_final_chunks_full" "stream_two_final_chunks_second"
       "stream_two_final_chunks_short" "version_unsupported" "x25519_bad_tag"
       "x25519_extra_argument" "x25519_identity" "x25519_long_file_key"
       "x25519_long_share" "x25519_low_order" "x25519_lowercase"
       "x25519_no_match" "x25519_not_canonical_body" "x25519_not_canonical_share"
       "x25519_short_share"))
(test-end)
