;;; goonix: the secrets service on a running system.
;;;
;;; A new file of ours; gnu/tests is not edited except for the marked hook
;;; that lists this module in gnu/local.mk.  The S-4 gate: a test system
;;; boots, the service reads its secret from /run/secrets with the declared
;;; owner and mode, and the canary is in no store item of that system.
;;;
;;; The machine's first identity reaches the guest through the first-boot
;;; copy service Guix already has for virtual machines (gnu build
;;; secret-service): the host makes a throwaway test identity, encrypts the
;;; canary to it, and sends the private half over the forwarded secrets port.
;;; The identity is made on the host at evaluation time and spliced into the
;;; test gexp, so it is in the test derivation's builder (a store item), but
;;; it is NOT in the guest system's closure, which is what the gate checks.
;;; The system closure carries only the ciphertext.
(define-module (gnu tests goonix-secrets)
  #:use-module (gnu tests)
  #:use-module (gnu image)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu system image)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu services goonix-secrets)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix self)
  #:use-module (guix secrets age)
  #:use-module (guix secrets vault)
  #:use-module (guix secrets materialise)
  #:use-module ((guix build utils) #:select (mkdir-p find-files))
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (%test-goonix-secrets))

(define %canary "goonix-s4-system-canary-4c1d8e2a-private")
(define %alice-canary "goonix-s4-system-alice-9b2f7c1d-private")
(define %secrets-port 11004)

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix rest ...) #t)
    (('gnu rest ...) #t)
    (_ #f)))

(define (test-vault identity)
  "A vault directory declaring two secrets encrypted to IDENTITY's public key:
one owned by root, one by the non-root user alice that the test system already
has.  The ciphertext is a store item; the canary plaintext is not."
  (let ((public (x25519-identity-public identity)))
    (file-union "secrets-vault"
                `(("vault.scm"
                   ,(plain-file "vault.scm"
                                (format #f "(vault (format 1) (recipients (machine ~s))
 (secrets (token (for machine) (owner \"root\") (mode #o400))
          (alice-token (for machine) (owner \"alice\") (mode #o400))))\n" public)))
                  ("token.age"
                   ,(plain-file "token.age"
                                (encrypt-age (string->utf8 %canary)
                                             (list public))))
                  ("alice-token.age"
                   ,(plain-file "alice-token.age"
                                (encrypt-age (string->utf8 %alice-canary)
                                             (list public))))))))

(define (run-secrets-test)
  (define identity (generate-x25519-identity))
  ;; Build the vault once, outside the operating-system form.  The services
  ;; field is thunked, so a (test-vault identity) call inside it would run
  ;; again on every read, and encrypt-age draws a new random file key each
  ;; time, giving a different vault, system and disk image on every read.
  (define vault (test-vault identity))
  ;; secret-service-type and secret-service-operating-system are not exported
  ;; from (gnu services virtualization); reference them directly.  The guest
  ;; listens for the first-boot copy on port 1004 through secret-service-type,
  ;; the way the childhurd does.
  (define secret-service-os
    (@@ (gnu services virtualization) secret-service-operating-system))
  (define os
    (marionette-operating-system
     (secret-service-os
      (operating-system
        (inherit (simple-operating-system))
        (services
         (cons* (service secrets-service-type
                         (secrets-configuration
                          (vault vault)
                          (first-boot-copy? #t)))
                ;; secret-service-operating-system modifies the openssh
                ;; service, so the system must have one.
                (service openssh-service-type
                         (openssh-configuration
                          (openssh openssh-sans-x)))
                (service static-networking-service-type
                         (list %qemu-static-networking))
                (operating-system-user-services (simple-operating-system))))))
     #:imported-modules '((gnu services herd))))

  ;; The guest listens for the first-boot copy on port 1004; forward it.
  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,%secrets-port . 1004)))))

  (define test
    (with-extensions (list guile-gcrypt guile-fibers)
      (with-imported-modules `(((guix config) => ,(make-config.scm))
                               ,@(source-module-closure
                                  '((gnu build marionette)
                                    (gnu build secret-service)
                                    (guix build store-copy)
                                    (guix build utils))
                                  #:select? not-config?))
        #~(begin
            (use-modules (gnu build marionette)
                         (gnu build secret-service)
                         (guix build store-copy)
                         (guix build utils)
                         (ice-9 textual-ports)
                         (ice-9 ftw)
                         (srfi srfi-1)
                         (srfi srfi-64))

            (define marionette
              (make-marionette (list #$vm)))

            (test-runner-current (system-test-runner #$output))
            (test-begin "goonix-secrets")

            ;; Send the machine's first identity through the first-boot copy.
            ;; The private half is written in the build directory and removed
            ;; afterwards; it is a throwaway test identity.
            (test-assert "first-boot identity copy arrives"
              (let* ((root (string-append (getenv "TMPDIR") "/first-boot"))
                     (identity-file
                      (string-append root "/etc/guix/secrets/identity.txt")))
                (mkdir-p (dirname identity-file))
                (call-with-output-file identity-file
                  (lambda (port)
                    (chmod identity-file #o600)
                    (display (string-append "# goonix machine age identity\n"
                                            #$identity "\n")
                             port)))
                (let ((sent (secret-service-send-secrets
                             (make-socket-address AF_INET INADDR_LOOPBACK
                                                  #$%secrets-port)
                             root)))
                  (delete-file-recursively root)
                  sent)))

            (test-assert "secrets service is running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'secrets))
               marionette))

            (test-equal "the secret is materialised with its plaintext"
              #$%canary
              (marionette-eval
               '(begin
                  (use-modules (ice-9 textual-ports))
                  (call-with-input-file "/run/secrets/token" get-string-all))
               marionette))

            (test-equal "the secret has the declared owner and mode"
              '(0 #o400)
              (marionette-eval
               '(let ((st (stat "/run/secrets/token")))
                  (list (stat:uid st) (stat:perms st)))
               marionette))

            (test-equal "/run/secrets is a symlink to a generation"
              #t
              (marionette-eval
               '(begin
                  (use-modules (guix build utils))
                  (and (symbolic-link? "/run/secrets")
                       (string-prefix? "/run/secrets.d/"
                                       (readlink "/run/secrets"))))
               marionette))

            (test-equal "the generation directory is traversable but not listable"
              #o751
              (marionette-eval
               '(logand (stat:perms (stat "/run/secrets")) #o777)
               marionette))

            ;; A secret declared for a non-root user is reachable by that user
            ;; through the 751 generation, and refused to another user.  The
            ;; read runs in a forked child that drops to the user's uid/gid,
            ;; the way the shadow and container tests do it.
            (test-equal "a non-root secret is read by its owner"
              #$%alice-canary
              (marionette-eval
               '(begin
                  (use-modules (ice-9 textual-ports) (ice-9 match))
                  ;; The guest REPL has no TMPDIR; use /tmp as the shadow test
                  ;; does.  The child drops to alice and writes what it reads.
                  ;; It is wrapped in dynamic-wind with a primitive-exit in the
                  ;; after thunk so that, if anything raises (setuid, getpwnam,
                  ;; or the read itself — the regression this check catches),
                  ;; the child still exits instead of answering on the REPL
                  ;; device and looping, which would leave two processes reading
                  ;; the virtio port and hang the parent's waitpid.
                  (let ((out "/tmp/goonix-alice-read"))
                    (match (primitive-fork)
                      (0
                       (dynamic-wind
                         (const #f)
                         (lambda ()
                           (setgroups #())
                           (setgid (passwd:gid (getpwnam "alice")))
                           (setuid (passwd:uid (getpwnam "alice")))
                           (call-with-output-file out
                             (lambda (port)
                               (display
                                (call-with-input-file "/run/secrets/alice-token"
                                  get-string-all)
                                port)))
                           (primitive-exit 0))
                         (lambda () (primitive-exit 127))))
                      (pid
                       (let ((status (cdr (waitpid pid))))
                         (and (zero? status)
                              (call-with-input-file out get-string-all)))))))
               marionette))

            (test-assert "a non-root secret is refused to another user"
              (marionette-eval
               '(begin
                  (use-modules (ice-9 match))
                  (match (primitive-fork)
                    (0
                     (dynamic-wind
                       (const #f)
                       (lambda ()
                         ;; Drop the REPL's (root's) supplementary groups too,
                         ;; so this really tests an unprivileged user.  nobody
                         ;; (uid 65534) owns nothing and may not read alice's
                         ;; secret.
                         (setgroups #())
                         (setgid 65534)
                         (setuid 65534)
                         (primitive-exit
                          (if (false-if-exception
                               (call-with-input-file "/run/secrets/alice-token"
                                 get-string-all))
                              1 0)))
                       (lambda () (primitive-exit 127))))
                    (pid (zero? (cdr (waitpid pid))))))
               marionette))

            ;; The gate: the canary is in no store item of the system.  The
            ;; reference graph of the system is read on the host (in this
            ;; build), so every item of the closure is checked, not just the
            ;; files of the system directory.  A dangling symlink (nss-certs
            ;; has a few) has no stat, so it is skipped, not an error.
            (test-assert "the canary is in no store item of the system"
              (let ((items (call-with-input-file "system-closure"
                             read-reference-graph)))
                (every (lambda (info)
                         (let ((item (store-info-item info)))
                           (every (lambda (file)
                                    (let ((st (stat file #f)))
                                      (or (not st)
                                          (not (eq? 'regular (stat:type st)))
                                          (let ((content
                                                 (call-with-input-file file
                                                   get-string-all)))
                                            (and (not (string-contains
                                                       content #$%canary))
                                                 (not (string-contains
                                                       content #$%alice-canary)))))))
                                  (if (file-is-directory? item)
                                      (find-files item)
                                      (list item)))))
                       items)))

            (test-end)))))
  (gexp->derivation "goonix-secrets-test" test
                    #:references-graphs `(("system-closure" ,os))))

(define %test-goonix-secrets
  (system-test
   (name "goonix-secrets")
   (description "Boot a system with the secrets service, send its first
identity through the first-boot copy, and check that the secret is
materialised in /run/secrets with its declared owner and mode while the
canary is in no store item of the system.")
   (value (run-secrets-test))))