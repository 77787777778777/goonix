;;; goonix: opt-in package addressing policy for CA-4.
(define-module (guix ca policy)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix ca)
  #:use-module (srfi srfi-1)
  #:export (%content-addressed-packages
            %package-addressing-policy
            content-addressed-package?
            package-addressing
            package-addressing-key
            derivation-with-addressing
            lower-with-addressing))

(define %content-addressed-packages
  ;; Package objects are compared by identity; no name-based guessing.
  (make-parameter '()))

(define %package-addressing-policy
  ;; The default is input addressing, so the feature is byte-for-byte inert.
  (make-parameter 'input))

(define (content-addressed-package? package)
  (or (eq? (%package-addressing-policy) 'content)
      (any (lambda (selected) (eq? selected package))
           (%content-addressed-packages))))

(define (package-addressing package)
  "Return the explicit addressing kind for PACKAGE."
  (let ((properties (package-properties package)))
    (cond ((eq? 'opaque (assq-ref properties 'self-references)) 'resolved)
          ((content-addressed-package? package) 'content)
          (else 'input))))

(define (derivation-with-addressing store drv addressing)
  ;; Preserve the original recipe and graph; only output addressing changes.
  (derivation store (derivation-name drv)
              (derivation-builder drv)
              (derivation-builder-arguments drv)
              #:system (derivation-system drv)
              #:inputs (derivation-inputs drv)
              #:sources (derivation-sources drv)
              #:env-vars (derivation-builder-environment-vars drv)
              #:outputs (derivation-output-names drv)
              #:addressing addressing))

(define %policy-keys (make-weak-key-hash-table))
(define %next-policy-key 0)

(define (package-addressing-key)
  "Return a stable key for the exact package-identity policy in effect."
  (let ((packages (%content-addressed-packages))
        (mode (%package-addressing-policy)))
    (or (hashq-ref %policy-keys packages)
        (let ((key (list mode (begin (set! %next-policy-key (+ 1 %next-policy-key))
                                    %next-policy-key))))
          (hashq-set! %policy-keys packages key)
          key))))

(define (lower-with-addressing package lower . arguments)
  "Lower PACKAGE with LOWER; selected packages are content-addressed."
  (let ((addressing (package-addressing package)))
    (if (eq? addressing 'input)
        (apply lower package arguments)
        (mlet %store-monad ((drv (apply lower package arguments)))
          ((store-lift (lambda (store drv)
                         (derivation-with-addressing store drv addressing)))
           drv)))))
