;;; goonix: the confined flake evaluator protocol.
(define-module (guix flakes evaluator)
  #:use-module (guix flakes support)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix build utils)
  #:use-module ((guix profiles)
                #:select (profile-derivation %default-profile-hooks))
  #:use-module ((gnu system) #:select (operating-system? operating-system-derivation))
  #:use-module ((gnu system image) #:select (system-image))
  #:use-module ((gnu image) #:select (image?))
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:export (run-flake-evaluator))

(define (lowered-output object name system target)
  "Return a monadic lowering computation for OBJECT."
  (match object
    ((? derivation? derivation) (with-monad %store-monad (return derivation)))
    ((? package? package)
     (lower-object package system #:target (or target #f)))
    ((? operating-system? os) (operating-system-derivation os))
    ((? image? image) (lower-object (system-image image)))
    ((? gexp? gexp)
     (gexp->derivation name gexp #:system system #:target target))
    ((? file-like? file) (lower-object file system #:target (or target #f)))
    (_ (error "output is not a package or other buildable object" object))))

(define (lower-value store object name system target overrides)
  (let ((active (remove (lambda (item)
                          (eq? object (override-replacement item)))
                        overrides)))
    (run-with-store store
      (with-package-overrides (map (lambda (item)
                                    (cons (override-original item)
                                          (override-replacement item)))
                                  active)
        (lowered-output object name system target)))))

(define (requested-shell outputs name development?)
  "Return what 'guix shell' enters for NAME among OUTPUTS, or #f: the shell
of that name, else the package of that name; with DEVELOPMENT?, the
development environment of the package of that name."
  (let ((shells (or (and=> (assq 'shells outputs) cdr) '()))
        (packages (or (and=> (assq 'packages outputs) cdr) '())))
    (if development?
        (and=> (assq name packages)
               (lambda (entry)
                 (shell (inputs-from (list (cdr entry))))))
        (and=> (or (assq name shells) (assq name packages)) cdr))))

(define (lower-shell store object system overrides bootstrap?)
  "Return the profile derivation of OBJECT, built the way 'guix shell' builds
the profile of a manifest.  BOOTSTRAP? has the meaning of its '--bootstrap'."
  (run-with-store store
    (with-package-overrides (map (lambda (item)
                                   (cons (override-original item)
                                         (override-replacement item)))
                                 overrides)
      (profile-derivation (shell->manifest object system)
                          #:system system
                          #:allow-collisions? #t
                          #:hooks (if bootstrap? '() %default-profile-hooks)
                          #:locales? (not bootstrap?)))
    #:guile-for-build
    (if bootstrap?
        (package-derivation store
                            (@ (gnu packages bootstrap) %bootstrap-guile)
                            system)
        (%guile-for-build))
    #:system system))

(define (write-shell store outputs request system overrides)
  "Answer a request for the environment of 'guix shell'."
  (let* ((name (assoc-ref request 'name))
         (options (assoc-ref request 'shell))
         (object (requested-shell outputs name
                                  (assoc-ref options 'development?))))
    (cond (object
           (write (list 'output 'shells name
                        (derivation-file-name
                         (lower-shell store object system overrides
                                      (assoc-ref options 'bootstrap?)))
                        `((shell . #t)
                          (environment . ,(if (shell? object)
                                              (shell-environment object)
                                              '())))))
           (newline) (force-output))
          ((assoc-ref options 'optional?)
           (write (list 'absent 'shells name))
           (newline) (force-output))
          (else
           (error "flake has neither a shell nor a package of this name"
                  name)))))

(define (metadata store object system target overrides)
  (cond ((package? object)
         `((version . ,(package-version object))
           (outputs . ,(package-outputs object))
           (search-paths . ,(map search-path-specification->sexp
                                 (package-search-paths object)))
           (propagated-inputs
            . ,(map (lambda (input)
                      (match input
                        ((label item . outputs)
                         (list label
                               (derivation-file-name
                                (lower-value store item label system target overrides))
                               outputs))))
                    (package-propagated-inputs object)))))
        ((shell? object)
         `((shell . #t)
           (environment . ,(shell-environment object))
           (packages . ,(map package-name (shell-packages object)))
           (inputs-from . ,(map package-name (shell-inputs-from object)))))
        ((derivation? object) `((outputs . ,(map car (derivation-outputs object)))))
        (else '())))

(define (run-flake-evaluator)
  "Read one data-only request and return serializable derivation handles."
  (guard (condition
          (else
           (write (list 'error (object->string condition)))
           (newline) (force-output)
           (raise-exception condition)))
    (run-flake-evaluator*)))

(define (run-flake-evaluator*)
  (let* ((request (read))
         (directory (assoc-ref request 'source))
         (manifest (call-with-input-file (string-append directory "/flake.scm") read))
         (output-form (match (assq 'outputs (cdr manifest))
                        ((_ form) form) (_ (error "missing outputs"))))
         (system (assoc-ref request 'system))
         (target (assoc-ref request 'target))
         (options (assoc-ref request 'options))
         (input-specs (or (and=> (assq 'inputs (cdr manifest)) cdr) '()))
         (input-sources (assoc-ref request 'input-sources))
         (inputs (map (lambda (spec)
                        (let ((name (car spec))
                              (kind (or (and=> (assq 'as (cdr spec)) cadr) 'flake)))
                          (cons name (list kind (assoc-ref input-sources name)))))
                      input-specs))
         (module (make-fresh-user-module))
         (s (open-connection))
         (context `((system . ,system) (target . ,target) (options . ,options)
                    (source . ,directory) (revision . ,(assoc-ref request 'revision))
                    (inputs . ,inputs))))
    (for-each
     (lambda (spec)
       (when (equal? '(as library) (assq 'as (cdr spec)))
         (set! %load-path (cons (car (assoc-ref input-sources (car spec))) %load-path))))
     input-specs)
    (module-use! module (resolve-interface '(guix flakes support)))
    (when (and (pair? output-form) (eq? 'lambda (car output-form)))
      (for-each (lambda (form)
                  (when (and (pair? form) (eq? 'use-modules (car form)))
                    (eval form module)))
                (cddr output-form)))
    (let* ((outputs ((eval output-form module) context))
           (kind (assoc-ref request 'kind))
           (name (assoc-ref request 'name))
           (overrides (append-map
                       (lambda (spec)
                         (if (eq? 'overrides (car spec))
                             (map cdr (cdr spec)) '()))
                       outputs)))
      (parameterize ((%current-system system) (%current-target-system target))
        (if (assoc-ref request 'shell)
            (write-shell s outputs request system overrides)
            (for-each
             (lambda (group)
               (for-each
                (lambda (entry)
                  (when (or (eq? kind '*) (eq? name (car entry)))
                    (let ((object (cdr entry)))
                      (write (list 'output (car group) (car entry)
                                   (derivation-file-name
                                    (lower-value s object
                                                 (symbol->string (car entry))
                                                 system target overrides))
                                   (metadata s object system target overrides)))
                      (newline) (force-output))))
                (cdr group)))
             (if (eq? kind '*)
                 outputs
                 (list (or (assq kind outputs)
                           (error "unknown flake output kind" kind)))))))
      (write '(done)) (newline) (force-output)
      (close-connection s))))
