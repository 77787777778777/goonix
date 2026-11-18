;;; goonix: evaluator-side vocabulary.  No host files are consulted.
(define-module (guix flakes support)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:export (option system target input-source input-output
            self self-file self-revision source-set shell
            input-handle? input-handle-path input-handle-metadata
            handle->derivation
            shell? shell-packages shell-inputs-from shell-environment
            override override? override-original override-replacement
            override-transform))

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
(define-record-type* <shell> shell make-shell shell?
  (packages shell-packages (default '()))
  (inputs-from shell-inputs-from (default '()))
  (environment shell-environment (default '())))

(define-record-type* <override> override make-override override?
  (original override-original)
  (replacement override-replacement))

(define-record-type <replacement>
  (make-replacement object original overrides)
  replacement?
  (object replacement-object)
  (original replacement-original)
  (overrides replacement-overrides))

(define (override-transform overrides)
  "Replace matching package objects at lowering time, in declaration order."
  (lambda (object)
    (let loop ((items overrides))
      (match items
        (() object)
        ((item . rest)
         (if (eq? object (override-original item))
             (make-replacement (override-replacement item)
                               (override-original item) overrides)
             (loop rest)))))))

(define-gexp-compiler (replacement-compiler (value <replacement>) system target)
  ;; A wrapper's reference to its original is exempt.  Other overrides still
  ;; apply while its dependency graph is lowered.
  (with-transformation
      (let ((original (replacement-original value))
            (overrides (replacement-overrides value))
            (transform (override-transform (replacement-overrides value))))
        (lambda (object)
          (if (eq? object original) object (transform object))))
    (lower-object (replacement-object value) system #:target target)))

(define-gexp-compiler (shell-compiler (object <shell>) system target)
  (let* ((packages (append (shell-packages object)
                           (append-map package-development-inputs
                                       (shell-inputs-from object))))
         (manifest (packages->manifest packages)))
    (profile-derivation manifest #:system system #:target target)))

(define (shell-manifest object)
  (packages->manifest (append (shell-packages object)
                              (append-map package-development-inputs
                                          (shell-inputs-from object)))))
