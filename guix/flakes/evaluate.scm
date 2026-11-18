;;; goonix: locked trees, contained processes, and serializable output handles.
(define-module (guix flakes evaluate)
  #:use-module (guix flakes)
  #:use-module (guix flakes checkout)
  #:use-module (guix pure-eval)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix build utils)
  #:autoload   (guix scripts build) (register-root)
  #:use-module ((guix utils) #:select (%current-system cache-directory))
  #:use-module ((guix config) #:select (%guile))
  #:use-module ((gnu build linux-container)
                #:select (run-container %writable-/tmp))
  #:use-module (gnu system file-systems)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (guix base16)
  #:use-module (gcrypt hash)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:export (%evaluation-cache evaluate-flake evaluate-flake-outputs flake-option-value
            evaluate-flake-shell
            parse-option-value evaluator-starts))

(define (parse-option-value option string)
  (case (flake-option-type option)
    ((boolean) (cond ((string=? string "true") #t)
                     ((string=? string "false") #f)
                     (else (error "expected boolean option value" string))))
    ((integer) (let ((n (string->number string)))
                 (unless (and n (integer? n)) (error "expected integer" string)) n))
    ((string) string)))

(define (validated-options flake options)
  (for-each (lambda (option)
              (unless (find (lambda (declared)
                              (eq? (car option) (flake-option-name declared)))
                            (flake-options flake))
                (error "undeclared flake option" (car option)))) options)
  (map (lambda (option)
         (cons (flake-option-name option)
               (match (assoc (flake-option-name option) options)
                 (#f (flake-option-default option))
                 ((_ . (? string? value)) (parse-option-value option value))
                 ((_ . value) value))))
       (flake-options flake)))

(define (mapping source)
  (file-system (device source) (mount-point source) (type "none")
               (flags '(bind-mount read-only)) (create-mount-point? #t)
               (check? #f)))

(define (exit-status status)
  (or (status:exit-val status) (+ 128 (or (status:term-sig status) 0))))

(define (write-file-string file contents)
  (call-with-output-file file (lambda (port) (display contents port))))

(define (locked-input-snapshot input)
  "Materialize and verify the tree recorded for INPUT in a committed lock."
  (let* ((source (case (locked-input-type input)
                   ((local) (locked-input-path input))
                   ((git) (locked-input-url input))
                   (else (error "unsupported locked input type" input))))
         (snapshot (case (locked-input-type input)
                     ((local)
                      (unless source (error "local lock has no source" input))
                      (tracked-tree source #:commit (locked-input-commit input)
                                    #:worktree? (locked-input-worktree? input)))
                     ((git)
                      (resolve-git-input source #:commit (locked-input-commit input))))))
    (unless (equal? (locked-input-commit input)
                    (tree-snapshot-commit snapshot))
      (error "checkout commit differs from lock" (locked-input-name input)))
    (unless (equal? (locked-input-tree-hash input)
                    (tree-snapshot-hash snapshot))
      (error "checkout content differs from lock" (locked-input-name input)))
    (verify-tree-snapshot snapshot)
    snapshot))

(define (locked-input-directory input)
  "Return INPUT's locked tree, never its later mutable working tree."
  (or (locked-input-checkout input)
      (tree-snapshot-directory (locked-input-snapshot input))))

(define evaluator-starts
  ;; Exported for tests; it counts confined process launches, not cache hits.
  (make-parameter 0))

(define* (evaluate-request store directory kind name #:key (options '())
                           (system (%current-system)) target (pure? #t)
                           imported-source overrides (path '()) shell)
  "Evaluate a locked tree in a fresh confined Guile and return output handles.
SHELL, when true, is an alist that asks for the environment of 'guix shell'
instead of an output of KIND; see 'evaluate-flake-shell'."
  (let* ((directory (canonicalize-path directory))
         (flake (read-flake (string-append directory "/flake.scm")))
         (lock (if (file-exists? (string-append directory "/flake.lock"))
                   (effective-flake-lock directory #:overrides (or overrides '()))
                   (if (or (null? (flake-inputs flake)) overrides)
                       (lock-flake directory #:local-overrides (or overrides '())
                                   #:write? #f)
                       (if pure?
                           (error "pure flake evaluation requires flake.lock; run guix flake lock first")
                           (lock-flake directory #:write? #f)))))
         (module-tree (import-guix-modules store))
         (support (add-file-tree-to-store
                   store `("support" directory
                           ("flakes" directory
                            ("support.scm" regular
                             (file ,(search-path %load-path "guix/flakes/support.scm")))))))
         (source (or imported-source
                     (if (and pure?
                              (not (string-prefix? (string-append (%store-prefix) "/")
                                                   directory)))
                         (import-tracked-tree store directory)
                         directory)))
         (manifest (call-with-input-file (string-append source "/flake.scm")
                     (lambda (port) (read port))))
         (declared (append
                   (or (and=> (assq 'inputs (cdr manifest)) cdr) '())
                   (map (lambda (input)
                          `(,(flake-input-name input) (as ,(flake-input-as input))))
                        (filter (lambda (input)
                                  (not (assoc (flake-input-name input)
                                              (or (and=> (assq 'inputs (cdr manifest)) cdr)
                                                  '()))))
                                (channel-inputs source)))))
         (locked-trees
          (map (lambda (spec)
                 (let* ((name (car spec))
                        (parent (or (and (null? path) #f)
                                    (find (lambda (input)
                                            (equal? path (locked-input-identity input)))
                                          (flake-lock-inputs lock))))
                        (edge (and parent
                                   (find (lambda (edge)
                                           (eq? name (lock-edge-name edge)))
                                         (locked-input-edges parent))))
                        (identity (if edge (lock-edge-child edge) (list name)))
                        (input (or (and edge (find (lambda (item)
                                                    (equal? identity (locked-input-identity item)))
                                                  (flake-lock-inputs lock)))
                                   (and (null? path)
                                        (find (lambda (item)
                                                (eq? name (locked-input-name item)))
                                              (flake-lock-inputs lock)))
                                   (error "missing locked input" name path))))
                   (let ((checkout (locked-input-directory input)))
                     (unless checkout (error "missing locked input checkout" name))
                     (cons name
                           (list (if pure?
                                     (add-to-store store "pure-eval-project" #t "sha256" checkout)
                                     checkout)
                                 (locked-input-options input)
                                 checkout identity)))))
               declared))
         (inputs (map (lambda (spec)
                        (let ((name (car spec))
                              (kind (or (and=> (assq 'as (cdr spec)) cadr) 'flake)))
                          (list name kind (or (and=> (assoc-ref locked-trees name) car)
                                              (error "missing input tree" name)))))
                      declared))
         (library-root (dirname (dirname (dirname %guile))))
         (runtime-path? (lambda (path)
                          (or (string-prefix? "/nix/store/" path)
                              (string-prefix? "/gnu/store/" path))))
         (load-path (append (list support module-tree source)
                            (filter runtime-path? %load-path)
                            (map caddr (filter (lambda (x) (eq? 'library (cadr x))) inputs))
                            (if (flake-modules flake)
                                (list (string-append source "/" (flake-modules flake))) '())))
         (compiled-path (append (list support module-tree)
                                (filter runtime-path? %load-compiled-path)))
         (mounts (append (list %writable-/tmp (mapping (%store-prefix))
                               (mapping library-root) (mapping (%daemon-socket-uri)))
                         (if pure? '() (list (mapping source)))))
         (root (mkdtemp "/tmp/goonix-evaluator-root.XXXXXX"))
         (result '()) (status #f))
    (dynamic-wind
      (const #t)
      (lambda ()
        (evaluator-starts (+ 1 (evaluator-starts)))
        (match (pipe) ((child-in . host-out)
          (match (pipe) ((host-in . child-out)
            (let ((pid (run-container root mounts '(user mnt pid uts ipc net) 1
                         (lambda ()
                           (close-port host-out) (close-port host-in)
                           (dup2 (fileno child-in) 0) (dup2 (fileno child-out) 1)
                           (close-port child-in) (close-port child-out)
                           (chdir source)
                           (setenv "HOME" "/tmp") (setenv "TMPDIR" "/tmp")
                           (setenv "GUIX_DAEMON_SOCKET" (%daemon-socket-uri))
                           (let ((environment
                                  (list "GUILE_AUTO_COMPILE=0" "HOME=/tmp" "TMPDIR=/tmp"
                                        (string-append "NIX_STORE_DIR=" (%store-prefix))
                                        (string-append "GUIX_DAEMON_SOCKET=" (%daemon-socket-uri))
                                        (string-append "GUILE_LOAD_PATH=" (string-join load-path ":"))
                                        (string-append "GUILE_LOAD_COMPILED_PATH=" (string-join compiled-path ":")))))
                             (apply execle %guile environment %guile "--no-auto-compile" "-c"
                                    "(use-modules (guix flakes evaluator)) (run-flake-evaluator)" '())))
                         #:loopback-network? #f)))
              (close-port child-in) (close-port child-out)
              (write `((source . ,source) (kind . ,kind) (name . ,name)
                       (system . ,system) (target . ,target)
                       (options . ,(validated-options flake options))
                       (revision . ,(and (not imported-source)
                                         (tracked-tree-revision directory)))
                       (shell . ,shell)
                       (input-sources . ,locked-trees)) host-out)
              (newline host-out) (force-output host-out)
              (let loop ((message (read host-in)))
                (if (eof-object? message)
                    #t
                    (match message
                      (('source-set files)
                       (write (list 'source (import-source-set store source files)) host-out)
                       (newline host-out) (force-output host-out)
                       (loop (read host-in)))
                      (('input-output input kind name)
                       (let ((child (assoc input locked-trees)))
                         (unless child (error "unknown locked input" input))
                         (let ((outputs (evaluate-request store (cadr child) kind name
                                                         #:options (caddr child) #:system system
                                                         #:target target #:pure? pure?
                                                         #:path (cadddr child))))
                           (match outputs
                             ((output)
                              (write (list 'handle (cadddr output) (car (cddddr output))) host-out))
                             (_ (error "dependency output did not return one handle" outputs)))))
                       (newline host-out) (force-output host-out)
                       (loop (read host-in)))
                      (('error description)
                       (error "confined flake evaluator error" description))
                      (('output . fields) (set! result (cons message result))
                                        (loop (read host-in)))
                      ;; An optional shell that the flake does not have.
                      (('absent . fields) (loop (read host-in)))
                      (('done) #t)
                      (_ (error "invalid evaluator message" message)))))
              (close-port host-out) (close-port host-in)
              (set! status (cdr (waitpid pid)))))))))
      (lambda () (delete-file-recursively root)))
    (unless (and status (zero? (exit-status status)))
      (error "flake evaluator failed" (and status (exit-status status))))
    (reverse result)))

(define %evaluation-cache
  ;; The on-disk cache survives Guix processes; this table avoids repeated I/O
  ;; within one command.  Impure evaluations are deliberately not cached.
  (make-hash-table))

(define (flake-cache-key directory source kind name options system target shell)
  (let* ((manifest (call-with-input-file (string-append source "/flake.scm")
                     get-string-all))
         (lock-file (string-append source "/flake.lock"))
         (lock (and (file-exists? lock-file)
                    (call-with-input-file lock-file get-string-all)))
         (guix (search-path %load-path "guix/flakes/evaluator.scm"))
         (guix-source (and guix (call-with-input-file guix get-string-all))))
    (list 1 manifest lock source kind name options system target shell
          guix-source)))

(define (flake-cache-files key)
  (let* ((digest (bytevector->base16-string
                  (sha256 (string->utf8 (object->string key)))))
         (directory (string-append (cache-directory) "/flake-eval")))
    (mkdir-p directory)
    (list (string-append directory "/" digest ".scm")
          (string-append directory "/" digest ".drv"))))

(define (cached-evaluation store key)
  (let ((datum (false-if-exception
                (call-with-input-file (car (flake-cache-files key)) read))))
    (match datum
      (('evaluation 1 stored-key outputs)
       (and (equal? stored-key key) (every (lambda (output)
                   (match output
                     (('output kind name path metadata)
                      (and (string? path) (string-suffix? ".drv" path)
                           (valid-path? store path)))
                     (_ #f)))
                 outputs)
          outputs))
    (_ #f))))

(define (store-evaluation! store key outputs)
  (match (flake-cache-files key)
    ((file root)
     (let ((temporary (string-append file ".tmp." (number->string (getpid)))))
       (call-with-output-file temporary
         (lambda (port)
           (write (list 'evaluation 1 key outputs) port)
           (newline port)))
       (rename-file temporary file)
       (for-each (lambda (output)
                   (match output
                     (('output kind name path metadata)
                      (let ((item (string-append root "-" (symbol->string kind)
                                                "-" (symbol->string name))))
                        (when (file-exists? item) (delete-file item))
                        (register-root store (list path) item)))
                     (_ #f)))
                 outputs)))))

(define* (evaluate-flake/cached store directory kind name source options system target overrides
                                #:key shell)
  (let ((overrides (or overrides '())))
    (if (and (null? overrides)
             (string-prefix? (string-append (%store-prefix) "/") source))
      (let* ((key (flake-cache-key directory source kind name options system target shell))
             (memory (hash-ref %evaluation-cache key)))
        (if (and memory (every (lambda (output)
                                 (valid-path? store (cadddr output)))
                               memory))
            memory
            (let ((cached (cached-evaluation store key)))
              (if cached
                  (begin (hash-set! %evaluation-cache key cached) cached)
                  (let ((outputs (evaluate-request store directory kind name
                                                  #:options options #:system system
                                                  #:target target #:pure? #t
                                                  #:imported-source source
                                                  #:shell shell)))
                    (store-evaluation! store key outputs)
                    (hash-set! %evaluation-cache key outputs)
                    outputs)))))
      (evaluate-request store directory kind name #:options options
                        #:system system #:target target #:pure? #f
                        #:overrides overrides #:shell shell))))

(define* (evaluate-flake/cache store directory kind name options system target pure? overrides
                               #:key shell)
  (let* ((directory (canonicalize-path directory))
         (source (if (and pure?
                           (not (string-prefix? (string-append (%store-prefix) "/")
                                                directory)))
                     (import-tracked-tree store directory)
                     directory)))
    (evaluate-flake/cached store directory kind name source options system target overrides
                           #:shell shell)))

(define* (evaluate-flake store directory kind name #:key (options '())
                         (system (%current-system)) target (pure? #t) overrides)
  (match (evaluate-flake/cache store directory kind name options system target pure? overrides)
    ((output) (read-derivation-from-file (canonicalize-path (cadddr output))))
    (_ (error "flake did not return one named output" kind name))))

(define* (evaluate-flake-shell store directory name
                               #:key development? optional? bootstrap?
                               (options '()) (system (%current-system)))
  "Evaluate in a container what 'guix shell' enters for NAME in the flake of
DIRECTORY: the shell of that name, else the package of that name, or with
DEVELOPMENT? the development environment of that package.  Return two
values: the derivation of its profile and the alist of the environment
variables it sets.  When there is none, return #f and the empty list if
OPTIONAL?, and raise an error otherwise.  BOOTSTRAP? has the meaning of the
'--bootstrap' option of 'guix shell'."
  (match (evaluate-flake/cache store directory 'shells name options system
                               #f #t #f
                               #:shell `((development? . ,development?)
                                         (optional? . ,optional?)
                                         (bootstrap? . ,bootstrap?)))
    ((('output _ _ path metadata))
     (values (read-derivation-from-file (canonicalize-path path))
             (or (assq-ref metadata 'environment) '())))
    (()
     (if optional?
         (values #f '())
         (error "flake has neither a shell nor a package of this name"
                name)))))

(define* (evaluate-flake-outputs store directory #:key (options '())
                                 (system (%current-system)) target (pure? #t) overrides)
  (map (lambda (message)
         (match message (('output kind name path metadata) (list kind name path))))
       (evaluate-flake/cache store directory '* #f options system target pure? overrides)))
