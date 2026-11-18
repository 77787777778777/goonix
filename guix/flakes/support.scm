;;; goonix: evaluator-side vocabulary.  No host files are consulted.
(define-module (guix flakes support)
  #:use-module (srfi srfi-9)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:export (option system target input-source input-output
            self self-file self-revision source-set shell
            input-handle? input-handle-path input-handle-metadata
            handle->derivation))

(define-record-type <input-handle>
  (make-input-handle path metadata)
  input-handle?
  (path input-handle-path)
  (metadata input-handle-metadata))

(define (handle->derivation store handle)
  (read-derivation-from-file (input-handle-path handle)))

(define-gexp-compiler input-handle-compiler <input-handle>
  compiler => (lambda (handle system target)
                (with-monad %store-monad
                  (return (read-derivation-from-file (input-handle-path handle)))))
  expander => (lambda (handle drv output)
                (derivation->output-path drv output)))

(define (option context name) (assoc-ref (assoc-ref context 'options) name))
(define (system context) (assoc-ref context 'system))
(define (target context) (assoc-ref context 'target))
(define (input-source context name) (cadr (assoc-ref (assoc-ref context 'inputs) name)))
(define (input-output context name kind output)
  (let ((value (assoc-ref (assoc-ref context 'inputs) name)))
    (unless value (error "unknown flake input" name))
    (unless (eq? 'flake (car value)) (error "not a flake input" name))
    ;; This is a request to the host; the response contains only a handle.
    (write (list 'input-output name kind output))
    (newline)
    (force-output)
    (let ((response (read)))
      (unless (and (pair? response) (eq? 'handle (car response)))
        (error "invalid dependency handle" response))
      (apply make-input-handle (cdr response)))))
(define (self context) context)
(define (self-file context name) (string-append (assoc-ref context 'source) "/" name))
(define (self-revision context) (assoc-ref context 'revision))
(define (source-set context . files)
  (write (list 'source-set files)) (newline) (force-output)
  (let ((response (read)))
    (unless (and (pair? response) (eq? 'source (car response)))
      (error "invalid source-set response" response))
    (cadr response)))
(define (shell . arguments) `((shell . ,arguments)))
