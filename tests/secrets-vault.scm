;;; goonix: vault data and private operations, independent of the CLI.
(use-modules (guix secrets age)
             (guix secrets vault)
             (guix build syscalls)
             ((guix build utils) #:select (delete-file-recursively))
             (rnrs bytevectors)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-64))

(define root (mkdtemp! "/tmp/goonix-vault-unit.XXXXXX"))
(define vault-path (string-append root "/vault"))
(define (read-text file) (call-with-input-file file get-string-all))
(define id (generate-x25519-identity))
(define public (x25519-identity-public id))
(mkdir vault-path)
(call-with-output-file (string-append vault-path "/vault.scm")
  (lambda (port)
    (format port "(vault (format 1) (recipients (owner ~s)) (secrets (guest (kind ssh-key) (for owner)) (value (for owner))))\n" public)))
(define parsed (call-with-input-file (string-append vault-path "/vault.scm") read-vault))
(add-secret vault-path parsed 'value 'file (string->utf8 "unit canary") id)

(test-begin "secrets-vault")
(test-equal "vault reads its declared secret" "unit canary"
  (utf8->string (show-secret vault-path parsed 'value id)))
(test-error "duplicate vault member is refused" #t
  (call-with-input-string "(vault (format 1) (format 1) (recipients) (secrets))" read-vault))
(test-error "trailing vault form is refused" #t
  (call-with-input-string "(vault (format 1) (recipients) (secrets)) (extra)" read-vault))
(test-error "reader evaluation is refused" #t
  (call-with-input-string "#.(begin (delete-file \"/tmp/should-not-exist\") (vault))" read-vault))
(test-assert "check refuses a tracked private key"
  (begin
    (call-with-output-file (string-append root "/leak")
      (lambda (port) (display (generate-x25519-identity) port)))
    (not (false-if-exception
          (check-vault vault-path parsed id #:project-directory root)))
    (delete-file (string-append root "/leak"))))
(test-assert "SSH generation writes an unencrypted OpenSSH pair"
  (begin
    (generate-ssh-key vault-path parsed 'guest id)
    (let ((secret (find (lambda (s) (eq? 'guest (secret-name s))) (vault-secrets parsed))))
      (and secret
           (string-prefix? "-----BEGIN OPENSSH PRIVATE KEY-----"
                           (utf8->string (show-secret vault-path parsed 'guest id)))
           (string-prefix? "ssh-ed25519 " (read-text (string-append vault-path "/guest.pub")))
           (list? (read-ssh-identity-text
                    (utf8->string (show-secret vault-path parsed 'guest id))))))))
(test-assert "check rejects a mismatched SSH public half"
  (let ((path (string-append vault-path "/guest.pub")))
    (call-with-output-file path (lambda (p) (display "ssh-ed25519 wrong\n" p)))
    (and (not (false-if-exception (check-vault vault-path parsed id)))
         (begin (generate-ssh-key vault-path parsed 'guest id) #t))))
(test-assert "exec exposes files and removes its directory"
  (let ((marker (string-append root "/directory")))
    (test-equal "exec command succeeds" 0
      (exec-with-secrets vault-path parsed id
                         (list "sh" "-c" "test -f \"$GUIX_SECRETS_DIRECTORY/value\" && printf %s \"$GUIX_SECRETS_DIRECTORY\" > \"$1\""
                               "sh" marker)))
    (let ((directory (read-text marker)))
      (and (not (file-exists? directory))
           (not (string-contains (read-text (string-append vault-path "/value.age")) "unit canary"))))))
(test-assert "exec failure removes its directory"
  (let ((marker (string-append root "/failure-directory")))
    (test-equal "exec preserves a failing command's status" 23
      (exec-with-secrets vault-path parsed id
                         (list "sh" "-c" "printf %s \"$GUIX_SECRETS_DIRECTORY\" > \"$1\"; exit 23"
                               "sh" marker)))
    (not (file-exists? (read-text marker)))))
(test-assert "rekey removes the old recipient's access"
  (let* ((new-id (generate-x25519-identity))
         (new-public (x25519-identity-public new-id))
         (new-vault (make-vault `((new . ,new-public))
                                (map (lambda (s)
                                       (make-secret (secret-name s) (secret-kind s)
                                                    '(new) (secret-owner s)
                                                    (secret-group s) (secret-mode s)))
                                     (vault-secrets parsed)))))
    (rekey-vault vault-path new-vault id)
    (and (not (false-if-exception (show-secret vault-path new-vault 'value id)))
         (equal? "unit canary" (utf8->string (show-secret vault-path new-vault 'value new-id))))))
(test-end)
(delete-file-recursively root)
