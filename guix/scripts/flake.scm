;;; goonix: data-first flake commands and the flake reference vocabulary.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.

(define-module (guix scripts flake)
  #:use-module (guix flakes)
  #:autoload   (guix flakes evaluate) (evaluate-flake evaluate-flake-outputs
                                       evaluate-flake-shell)
  #:autoload   (guix flakes checkout) (tracked-tree resolve-git-input
                                       tree-snapshot-directory)
  #:autoload   (guix pure-eval) (import-tracked-tree import-source-set
                                 pure-eval-supported?)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (guix diagnostics)
  #:use-module ((guix utils) #:select (%current-system))
  #:autoload   (gnu system) (operating-system? operating-system-derivation)
  #:autoload   (gnu home) (home-environment? home-environment-derivation)
  #:autoload   (gnu image) (image?)
  #:autoload   (gnu system image) (system-image)
  #:autoload   (gnu machine) (machine?)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:export (guix-flake
            flake-reference?
            parse-flake-reference
            flake-options-from-opts
            flake-output-derivation
            flake-package-derivation
            flake-acting-output
            flake-checked-output
            lower-flake-object
            resolve-flake-shell-reference
            flake-default-shell-options))

(define (show-help)
  (display (G_ "Usage: guix flake COMMAND [OPTION...] [DIRECTORY]
Lock, inspect, and evaluate a goonix flake.

  lock                    resolve inputs without running project code
  update [INPUT]          update the lock, or only INPUT
  metadata                print the locked dependency graph as data
  show                    list declared outputs
  check                   build the flake's checks
  init                    create a flake.scm

  --override-input=NAME=SPEC
                          use a local input for this invocation only
  --option=NAME=VALUE    set a declared evaluation option
  --system=SYSTEM         evaluate for SYSTEM
  --target=TARGET         cross evaluate for TARGET
  -h, --help              display this help and exit
  -V, --version           display version information and exit\n")))

(define (option-value string)
  (let ((equal (string-index string #\=)))
    (unless equal (leave (G_ "expected NAME=VALUE, got '~a'~%") string))
    (values (string->symbol (string-take string equal))
            (string-drop string (+ equal 1)))))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda (opt name arg result)
                  (leave-on-EPIPE (show-help))
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda _ (show-version-and-exit "guix flake")))
        (option '("override-input") #t #f
                (lambda (opt name arg result)
                  (let-values (((name value) (option-value arg)))
                    (alist-cons 'override (cons name value) result))))
        (option '("option") #t #f
                (lambda (opt name arg result)
                  (let-values (((name value) (option-value arg)))
                    (alist-cons 'option (cons name value) result))))
        (option '("system") #t #f
                (lambda (opt name arg result) (alist-cons 'system arg result)))
        (option '("target") #t #f
                (lambda (opt name arg result) (alist-cons 'target arg result)))))

(define (parse-override value)
  (let ((data (read-data-string value)))
    (match data
      ((('local path)) (list (list 'local path)))
      ((('git url) rest ...) (cons (list 'git url) rest))
      (((or 'local 'git) source rest ...) (cons (list (car data) source) rest))
      (_ (leave (G_ "invalid local input override: ~s~%") value)))))

(define (positional-arguments options)
  "Return the non-option arguments recorded in OPTIONS, in order."
  (reverse (filter-map (match-lambda (('argument . value) value) (_ #f))
                       options)))

(define (directory-of options)
  (match (positional-arguments options)
    (() (getcwd))
    ((directory) directory)
    (_ (leave (G_ "only one flake directory may be specified~%")))))

(define (overrides-of options)
  (map (match-lambda
         ((name . value) (cons name (parse-override value))))
       (reverse (filter-map (match-lambda (('override . pair) pair) (_ #f))
                            options))))

(define (flake-options-from-opts opts)
  "Extract the declared-option alist from a command-line option alist OPTS."
  (filter-map (match-lambda (('option . pair) pair) (_ #f)) opts))

;;;
;;; Flake references: PATH-OR-URL#NAME.
;;;

(define (flake-reference? spec)
  "Whether SPEC is a flake reference of the form PATH#NAME."
  (and (string? spec)
       (let ((hash (string-index spec #\#)))
         (and hash (> hash 0) (< hash (- (string-length spec) 1))
              (let ((path (string-take spec hash)))
                (or (string=? path ".") (string=? path "..")
                    (string-prefix? "/" path)
                    (string-prefix? "./" path)
                    (string-prefix? "../" path)))))))

(define (parse-flake-reference spec)
  "Split SPEC into (DIRECTORY . NAME), NAME a symbol.  A reference without a
#NAME part yields NAME #f, which callers resolve to the kind's default."
  (let ((hash (string-index spec #\#)))
    (if hash
        (cons (canonicalize-path (string-take spec hash))
              (string->symbol (string-drop spec (+ hash 1))))
        (cons (canonicalize-path spec) #f))))

(define (flake-package-derivation store spec opts)
  "Return the derivation of the package that SPEC, a flake reference, names,
for 'guix build' with options OPTS."
  (match (parse-flake-reference spec)
    ((directory . name)
     (flake-output-derivation store directory 'packages name opts))))

(define (flake-output-derivation store directory kind name opts)
  "Purely evaluate DIRECTORY's output KIND NAME and return its derivation."
  (evaluate-flake store directory kind name
                  #:options (flake-options-from-opts opts)
                  #:system (or (assoc-ref opts 'system) (%current-system))
                  #:target (assoc-ref opts 'target)))

;;;
;;; Acting evaluation.
;;;
;;; Commands that change the host (system/home reconfigure, deploy) and the
;;; typed shell need the real Scheme object, not just a derivation.  We
;;; re-run the flake's outputs procedure in this process, over the same
;;; imported tracked tree and locked inputs the pure evaluator uses, and
;;; return the object itself.  Because it runs outside the container it can
;;; see ambient state; that is exactly what the derivation comparison below
;;; is there to catch.
;;;

(define (acting-input-trees store directory)
  "Return an alist mapping each locked input name to (CHECKOUT OPTIONS),
where CHECKOUT is the input's locked tree on disk."
  (let ((lock-file (string-append directory "/flake.lock")))
    (if (file-exists? lock-file)
        (map (lambda (input)
               (cons (locked-input-name input)
                     (list (or (locked-input-checkout input)
                               (tree-snapshot-directory
                                (case (locked-input-type input)
                                  ((local)
                                   (tracked-tree (locked-input-path input)
                                                 #:commit (locked-input-commit input)
                                                 #:worktree? (locked-input-worktree? input)))
                                  ((git)
                                   (resolve-git-input (locked-input-url input)
                                                      #:commit (locked-input-commit input))))))
                           (locked-input-options input))))
             (flake-lock-inputs (effective-flake-lock directory)))
        '())))

(define (acting-source-set store source files)
  (import-source-set store source files))

(define (acting-input-handle store trees name kind output system target)
  "Resolve a dependency output to a derivation handle, as the confined
evaluator's host side does."
  (let ((entry (assoc name trees)))
    (unless entry (error "unknown locked input" name))
    (let ((drv (evaluate-flake store (car (cadr entry)) kind output
                               #:options (cadr (cadr entry))
                               #:system system #:target target)))
      (list 'handle (derivation-file-name drv) '()))))

(define (call-with-acting-ports store source trees system target thunk)
  "Run THUNK with current input/output ports wired to an in-process handler
for the evaluator's source-set and input-output requests."
  (define out-buffer "")
  (define in-buffer "")
  (define in-position 0)
  (define (handle message)
    (match message
      (('source-set files)
       (list 'source (acting-source-set store source files)))
      (('input-output name kind output)
       (acting-input-handle store trees name kind output system target))
      (_ (error "invalid acting evaluator request" message))))
  (define (try-process)
    (let ((message (false-if-exception
                    (call-with-input-string out-buffer read))))
      (when (and message (not (eof-object? message)))
        (set! out-buffer "")
        (set! in-buffer
              (call-with-output-string
                (lambda (port) (write (handle message) port) (newline port))))
        (set! in-position 0))))
  (define output-port
    (make-soft-port
     (vector (lambda (c) (set! out-buffer (string-append out-buffer (string c)))
               (try-process))
             (lambda (s) (set! out-buffer (string-append out-buffer s))
               (try-process))
             (lambda () (try-process))
             (lambda () #f)
             (lambda () #f))
     "w"))
  (define input-port
    (make-soft-port
     (vector (lambda (c) #f)
             (lambda (s) #f)
             (lambda () #f)
             (lambda ()
               (if (< in-position (string-length in-buffer))
                   (let ((c (string-ref in-buffer in-position)))
                     (set! in-position (+ 1 in-position))
                     c)
                   #f))
             (lambda () #f))
     "r"))
  (parameterize ((current-input-port input-port)
                 (current-output-port output-port))
    (thunk)))

(define (call-with-flake-outputs store directory opts proc)
  "Set up DIRECTORY's outputs procedure and locked inputs, then call PROC
with (OUTPUT-THUNK SOURCE TREES SYSTEM TARGET).  OUTPUT-THUNK evaluates the
outputs procedure and returns the full alist of real Scheme objects; the
caller decides whether to run it in this process or in a container."
  (let* ((directory (canonicalize-path directory))
         (flake (read-flake (string-append directory "/flake.scm")))
         (system (or (assoc-ref opts 'system) (%current-system)))
         (target (assoc-ref opts 'target))
         (options (map (lambda (option)
                         (cons (flake-option-name option)
                               (match (assoc (flake-option-name option)
                                             (flake-options-from-opts opts))
                                 (#f (flake-option-default option))
                                 ((_ . (? string? value))
                                  ((@ (guix flakes evaluate) parse-option-value)
                                   option value))
                                 ((_ . value) value))))
                       (flake-options flake)))
         (source (import-tracked-tree store directory))
         (trees (acting-input-trees store directory))
         (manifest (call-with-input-file (string-append source "/flake.scm") read))
         (output-form (match (assq 'outputs (cdr manifest))
                        ((_ form) form) (_ (error "missing outputs"))))
         (input-specs (or (and=> (assq 'inputs (cdr manifest)) cdr) '()))
         (inputs (map (lambda (spec)
                        (let ((name (car spec))
                              (kind (or (and=> (assq 'as (cdr spec)) cadr) 'flake)))
                          (cons name (list kind
                                           (car (or (assoc name trees)
                                                    (error "missing input tree" name)))))))
                      input-specs))
         (module (make-fresh-user-module))
         (context `((system . ,system) (target . ,target) (options . ,options)
                    (source . ,source) (revision . #f) (inputs . ,inputs))))
    ;; Library inputs contribute their modules to the load path, as in the
    ;; confined evaluator.
    (for-each (lambda (spec)
                (when (equal? '(as library) (assq 'as (cdr spec)))
                  (set! %load-path
                        (cons (car (assoc-ref inputs (car spec))) %load-path))))
              input-specs)
    (when (flake-modules flake)
      (set! %load-path (cons (string-append source "/" (flake-modules flake))
                             %load-path)))
    (module-use! module (resolve-interface '(guix flakes support)))
    (when (and (pair? output-form) (eq? 'lambda (car output-form)))
      (for-each (lambda (form)
                  (when (and (pair? form) (eq? 'use-modules (car form)))
                    (eval form module)))
                (cddr output-form)))
    (proc (lambda ()
            (call-with-acting-ports store source trees system target
              (lambda ()
                (parameterize ((%current-system system)
                               (%current-target-system target))
                  ((eval output-form module) context)))))
          source trees system target)))

(define (acting-outputs store directory opts)
  "Evaluate DIRECTORY's outputs procedure in this process and return the
full output alist of real Scheme objects."
  (call-with-flake-outputs store directory opts
    (lambda (evaluate source trees system target)
      (evaluate))))

(define (flake-acting-output store directory kind name opts)
  "Return the real Scheme object for DIRECTORY's output KIND NAME, evaluated
in this (acting) process.  NAME #f selects the first entry of the kind."
  (let* ((outputs (acting-outputs store directory opts))
         (group (or (assq kind outputs)
                    (error "unknown flake output kind" kind)))
         (entry (if name
                    (or (assq name (cdr group))
                        (error "unknown flake output" kind name))
                    (match (cdr group)
                      (() (error "flake output kind is empty" kind))
                      ((first . _) first)))))
    (cdr entry)))

(define (lower-flake-object store object name system target)
  "Lower a real flake output OBJECT to a derivation, mirroring the confined
evaluator's lowering."
  (cond ((derivation? object) object)
        ((package? object)
         (if target
             (package-cross-derivation store object target system)
             (package-derivation store object system)))
        ((operating-system? object)
         (run-with-store store (operating-system-derivation object)
                         #:system system #:target target))
        ((home-environment? object)
         (run-with-store store (home-environment-derivation object)
                         #:system system #:target target))
        ((image? object)
         (run-with-store store (lower-object (system-image object))
                         #:system system #:target target))
        ((gexp? object)
         (run-with-store store (gexp->derivation name object
                                                 #:system system #:target target)))
        ((file-like? object)
         (run-with-store store (lower-object object)
                         #:system system #:target target))
        (else (error "output is not a buildable object" object))))

(define (flake-checked-output store directory kind name opts)
  "Evaluate KIND NAME purely and again in the acting process; refuse to
return the acting object unless both lowering paths agree on the derivation.
This is the purity gate for host-changing commands."
  (let* ((system (or (assoc-ref opts 'system) (%current-system)))
         (target (assoc-ref opts 'target))
         (pure (flake-output-derivation store directory kind name opts))
         (object (flake-acting-output store directory kind name opts))
         (acting (lower-flake-object store object
                                     (symbol->string (or name kind))
                                     system target)))
    (unless (string=? (derivation-file-name pure)
                      (derivation-file-name acting))
      (leave (G_ "flake output '~a.~a' is impure: the acting derivation ~a \
differs from the pure derivation ~a; refusing to act~%")
             kind (or name 'default)
             (derivation-file-name acting)
             (derivation-file-name pure)))
    object))

(define (flake-shell-reference? spec)
  "Whether SPEC, an argument of 'guix shell', names a flake: DIRECTORY#NAME,
or a directory given as a path that holds a flake.scm."
  (and (string? spec)
       (or (flake-reference? spec)
           (and (not (string-index spec #\#))
                (or (member spec '("." ".."))
                    (string-prefix? "./" spec)
                    (string-prefix? "../" spec)
                    (string-prefix? "/" spec))
                (file-exists? (string-append spec "/flake.scm"))))))

(define (flake-shell-options directory name opts development? optional?)
  "Evaluate in a container what 'guix shell' enters for NAME in the flake of
DIRECTORY, build its profile, and return the options that stand for it: the
profile and the environment variables the shell sets.  Return #f when
OPTIONAL? and the flake has nothing of that name.  The flake's code never
runs in this process."
  (unless (pure-eval-supported?)
    (leave (G_ "a flake shell is evaluated in a container, which needs \
unprivileged user namespaces~%")))
  (with-status-verbosity (or (assoc-ref opts 'verbosity) 1)
    (with-store store
      (set-build-options-from-command-line store opts)
      (let-values (((drv environment)
                    (evaluate-flake-shell
                     store directory name
                     #:development? development?
                     #:optional? optional?
                     #:bootstrap? (assoc-ref opts 'bootstrap?)
                     #:options (flake-options-from-opts opts)
                     #:system (or (assoc-ref opts 'system)
                                  (%current-system)))))
        (and drv
             (let ((dry-run? (assoc-ref opts 'dry-run?)))
               (with-build-handler
                   (build-notifier #:use-substitutes?
                                   (assoc-ref opts 'substitutes?)
                                   #:verbosity (assoc-ref opts 'verbosity)
                                   #:dry-run? dry-run?)
                 (build-derivations store (list drv)))
               (when dry-run?
                 (exit 0))
               (cons (cons 'profile (derivation->output-path drv))
                     (map (match-lambda
                            ((variable . value)
                             (cons 'environment-variables
                                   (cons variable value))))
                          environment))))))))

(define (resolve-flake-shell-reference opts)
  "If the packages named in OPTS, the options of 'guix shell', include a
flake reference, return OPTS with that reference replaced by the profile of
the shell it names.  With '-D' the reference names a package and stands for
its development environment.  Return OPTS unchanged otherwise."
  (define (payload? option)
    (match option
      (((or 'package 'load 'manifest 'profile 'expression) . _) #t)
      (_ #f)))

  (define (reference? option)
    (match option
      (('package _ (? flake-shell-reference?)) #t)
      (_ #f)))

  (match (filter reference? opts)
    (() opts)
    ((('package mode spec))
     (unless (= 1 (count payload? opts))
       (leave (G_ "'~a': a flake shell cannot be combined with other \
packages, files or profiles~%")
              spec))
     (match (parse-flake-reference spec)
       ((directory . name)
        (append (flake-shell-options directory (or name 'default) opts
                                     (eq? mode 'package) #f)
                (remove reference? opts)))))
    ((_ (_ _ spec) . _)
     (leave (G_ "'~a': only one flake shell can be entered~%") spec))))

(define (flake-default-shell-options file opts)
  "Return OPTS, the options of 'guix shell', extended with the default shell
of the flake that FILE, a flake.scm, defines.  This is what 'guix shell'
without arguments enters."
  (let ((directory (dirname (canonicalize-path file))))
    (append (or (flake-shell-options directory 'default opts #f #t)
                (leave (G_ "'~a' has no shell named 'default'~%") file))
            opts)))

;;;
;;; Command entry point.
;;;

(define %starter-flake
  "(flake (format 1)
 (description \"A goonix flake\")

 ;; Inputs are data; locking resolves them without running project code.
 (inputs)

 (outputs
  (lambda (in)
    ;; Return an alist of output kinds.  For example:
    ;;   `((packages (hello . ,(specification->package \"hello\"))))
    '())))
")

(define (flake-check store directory opts)
  "Evaluate every declared output, then build the flake's checks."
  (let* ((outputs (evaluate-flake-outputs store directory
                                          #:options (flake-options-from-opts opts)
                                          #:system (or (assoc-ref opts 'system)
                                                       (%current-system))
                                          #:target (assoc-ref opts 'target)))
         (checks (filter (lambda (entry) (eq? 'checks (car entry))) outputs)))
    (if (null? checks)
        (info (G_ "no checks declared; all outputs evaluated successfully~%"))
        (build-derivations store
                           (map (lambda (entry)
                                  (read-derivation-from-file (caddr entry)))
                                checks)))))

(define-command (guix-flake . args)
  (category development)
  (synopsis "lock, inspect, and evaluate goonix flakes")
  (match args
    (() (show-help) (exit 1))
    (((or "--help" "-h") . _) (show-help) (exit 0))
    ((command arguments ...)
     (let* ((options (parse-command-line arguments %options (list '())
                                         #:build-options? #f))
            (overrides (overrides-of options)))
       (case (string->symbol command)
         ((lock)
          (let ((directory (directory-of options)))
            (lock-flake directory #:local-overrides overrides)
            (format #t "~a~%" (string-append directory "/flake.lock"))))
         ((update)
          (let* ((names (positional-arguments options))
                 (directory (if (and (pair? names)
                                     (file-exists? (string-append (last names)
                                                                  "/flake.scm")))
                                (last names)
                                (getcwd)))
                 (inputs (if (and (pair? names)
                                  (file-exists? (string-append (last names)
                                                               "/flake.scm")))
                             (drop-right names 1)
                             names)))
            (update-flake directory
                          (if (null? inputs) #t (map string->symbol inputs)))
            (format #t "~a~%" (string-append directory "/flake.lock"))))
         ((metadata)
          (let ((directory (directory-of options)))
            (pretty-print (flake-metadata (effective-flake-lock directory
                                                                #:overrides overrides)))))
         ((show)
          (let ((directory (directory-of options)))
            (with-store store
              (for-each
               (match-lambda
                 ((kind name path)
                  (format #t "~a ~a ~a~%" kind name path)))
               (evaluate-flake-outputs store directory
                                      #:options (flake-options-from-opts options)
                                      #:system (or (assoc-ref options 'system)
                                                   (%current-system))
                                      #:target (assoc-ref options 'target)
                                      #:overrides overrides)))))
         ((check)
          (let ((directory (directory-of options)))
            (with-store store
              (set-build-options-from-command-line store options)
              (flake-check store directory options))))
         ((init)
          (let* ((directory (directory-of options))
                 (file (string-append directory "/flake.scm")))
            (when (file-exists? file)
              (leave (G_ "'~a' already exists~%") file))
            (call-with-output-file file
              (lambda (port) (display %starter-flake port)))
            (format #t "~a~%" file)))
         (else (leave (G_ "unknown flake command '~a'~%") command)))))))
