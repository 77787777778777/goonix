;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix pure-eval)
  #:use-module ((guix config) #:select (%guile))
  #:use-module (guix store)
  #:use-module ((guix utils)
                #:select (%current-system call-with-temporary-directory))
  #:use-module (guix build utils)
  #:use-module ((gnu build linux-container)
                #:select (call-with-container %writable-/tmp
                          user-namespace-supported?
                          unprivileged-user-namespace-supported?
                          setgroups-supported?))
  #:use-module (gnu system file-systems)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:export (pure-eval-supported?
            import-tracked-tree
            tracked-tree-revision
            import-source-set
            import-guix-modules
            call-with-pure-eval
            load-file-purely))

(define (pure-eval-supported?)
  "Whether evaluation can be confined with unprivileged Linux namespaces."
  (and (user-namespace-supported?)
       (unprivileged-user-namespace-supported?)
       (setgroups-supported?)))

(define (tracked-tree-revision directory)
  "Return the commit of DIRECTORY, or #f when tracked contents have changed."
  (and (file-exists? (string-append directory "/.git"))
       (let* ((port (open-pipe* OPEN_READ "git" "-C" directory
                               "status" "--porcelain" "--untracked-files=no"
                               "--ignore-submodules=all" "--" "."))
              (changes (get-string-all port))
              (status (close-pipe port)))
         (and (zero? status) (string-null? (string-trim-both changes))
              (let* ((port (open-pipe* OPEN_READ "git" "-C" directory
                                      "rev-parse" "HEAD"))
                     (revision (get-string-all port))
                     (status (close-pipe port)))
                (and (zero? status) (string-trim-right revision)))))))

(define (tree-files directory)
  (let walk ((root directory) (prefix ""))
    (append-map
     (lambda (name)
       (if (member name '("." ".."))
           '()
           (let* ((relative (if (string-null? prefix) name
                                (string-append prefix "/" name)))
                  (file (string-append root "/" name)))
             (if (eq? 'directory (stat:type (lstat file)))
                 (walk file relative)
                 (list relative)))))
     (scandir root))))

(define (tracked-tree directory)
  "Materialize the working contents of DIRECTORY's tracked files.
The file names come from Git's index; contents come from the working tree.
Untracked and ignored files are never read, and no host directory is exposed
to the evaluator.  The caller owns the returned temporary directory."
  (let ((temporary (mkdtemp (string-append (or (getenv "TMPDIR") "/tmp")
                                           "/goonix-pure-tree.XXXXXX"))))
    (dynamic-wind
      (const #t)
      (lambda ()
        (let* ((port (open-pipe* OPEN_READ "git" "-C" directory
                                 "ls-files" "-z" "--" "."))
               (names (string-split (get-string-all port) #\nul))
               (status (close-pipe port)))
          (unless (zero? status)
            (error "cannot enumerate tracked project tree" directory))
          (for-each
           (lambda (name)
             (unless (string-null? name)
               (let ((source (string-append directory "/" name))
                     (target (string-append temporary "/" name)))
                 (mkdir-p (dirname target))
                 (when (file-exists? source)
                   (if (eq? 'symlink (stat:type (lstat source)))
                       (symlink (readlink source) target)
                       (begin (copy-file source target)
                              (chmod target (stat:mode (lstat source)))))))))
           names))
        temporary)
      (lambda () #f))))

(define (import-tracked-tree store directory)
  "Import DIRECTORY's tracked files into STORE and return the store path."
  (let ((tree (tracked-tree directory)))
    (dynamic-wind
      (const #t)
      (lambda () (add-to-store store "pure-eval-project" #t "sha256" tree))
      (lambda () (delete-file-recursively tree)))))

(define (tracked-file-set directory files)
  "Select FILES from DIRECTORY, which may already be an imported store tree."
  (let* ((temporary (if (string-prefix? (string-append (%store-prefix) "/")
                                        (canonicalize-path directory))
                       #f
                       (tracked-tree directory)))
        (source-root (if (string-prefix? (string-append (%store-prefix) "/")
                                        (canonicalize-path directory))
                         (canonicalize-path directory)
                         temporary)))
    (for-each
     (lambda (file)
       (unless (and (string? file) (not (string-null? file))
                    (not (string-prefix? "/" file))
                    (not (member "" (string-split file #\/)))
                    (not (member ".." (string-split file #\/)))
                    (not (member "." (string-split file #\/))))
         (error "invalid source-set file" file))
       (let ((path (string-append source-root "/" file)))
         (unless (and (file-exists? path) (not (eq? 'directory (stat:type (lstat path)))))
           (error "source-set file is not tracked" file))))
     files)
    (dynamic-wind
      (const #t)
      (lambda ()
        (let ((selected (mkdtemp (string-append (or (getenv "TMPDIR") "/tmp")
                                               "/goonix-source-set.XXXXXX"))))
          (for-each
           (lambda (file)
             (let ((source (string-append source-root "/" file))
                   (target (string-append selected "/" file)))
               (mkdir-p (dirname target))
               (if (eq? 'symlink (stat:type (lstat source)))
                   (symlink (readlink source) target)
                   (begin (copy-file source target)
                          (chmod target (stat:mode (lstat source)))))))
           files)
          selected))
      (lambda () (when temporary (delete-file-recursively temporary))))))

(define (import-source-set store directory files)
  "Import exactly the selected tracked FILES from DIRECTORY into STORE."
  (let ((tree (tracked-file-set directory files)))
    (dynamic-wind
      (const #t)
      (lambda () (add-to-store store "source-set" #t "sha256" tree))
      (lambda () (delete-file-recursively tree)))))

(define* (import-guix-modules store)
  "Import the active Guix worktree's tracked Guix and GNU modules."
  (let* ((module (search-path %load-path "guix/flakes/evaluator.scm"))
         (root (if module
                   (dirname (dirname (dirname (canonicalize-path module))))
                   (getcwd)))
         (temporary (mkdtemp (string-append (or (getenv "TMPDIR") "/tmp")
                                            "/goonix-guix-modules.XXXXXX"))))
    (dynamic-wind
      (const #t)
      (lambda ()
        (let ((tree (tracked-tree root)))
          (for-each
           (lambda (file)
             (when (or (member file '("guix.scm" "gnu.scm"))
                       (string-prefix? "guix/" file)
                       (string-prefix? "gnu/" file))
               (let ((source (string-append root "/" file))
                     (target (string-append temporary "/" file)))
                 (mkdir-p (dirname target))
                 (if (eq? 'symlink (stat:type (lstat source)))
                     (symlink (readlink source) target)
                     (copy-file source target))
                 (when (string-suffix? ".scm" file)
                   (let* ((compiled-name (string-append (dirname file) "/"
                                                       (basename file ".scm") ".go"))
                          (compiled (string-append root "/" compiled-name))
                          (target-compiled (string-append temporary "/" compiled-name)))
                     (when (file-exists? compiled)
                       (copy-file compiled target-compiled)))))))
           (tree-files tree))
          (delete-file-recursively tree))
        (for-each
         (lambda (file)
           (let ((source (string-append root "/" file))
                 (target (string-append temporary "/" file)))
             (when (file-exists? source)
               (mkdir-p (dirname target))
               (copy-file source target))))
         '("guix/config.scm" "guix/config.go"))
        (add-to-store store "pure-eval-guix-modules" #t "sha256" temporary))
      (lambda () (delete-file-recursively temporary)))))

(define* (mapping source #:optional (target source))
  (file-system
    (device source)
    (mount-point target)
    (type "none")
    (flags '(bind-mount read-only))
    (create-mount-point? #t)
    (check? #f)))

(define (exit-status status)
  (or (status:exit-val status)
      (+ 128 (or (status:term-sig status) 0))))

(define* (call-with-pure-eval store load-paths thunk
                              #:key evaluate
                              (socket (%daemon-socket-uri))
                              (directory #f)
                              (compiled-paths %load-compiled-path))
  "Evaluate THUNK in a confined process and return its serializable result.
The evaluator gets a fresh namespace, not the caller's working tree, home,
network, or environment.  LOAD-PATHS must name only locked/store trees."
  (unless (pure-eval-supported?)
    (error "pure evaluation requires unprivileged user namespaces"))
  (match (pipe)
    ((input . output)
     (let* ((mounts (cons* %writable-/tmp
                           (mapping (%store-prefix))
                           (mapping socket)
                           (map mapping load-paths)))
            (status (false-if-exception
                     (call-with-container mounts
                      (lambda ()
                        (close-port input)
                        (with-exception-handler
                            (lambda (condition)
                              (false-if-exception
                               (begin
                                 (write (list 'error
                                              (call-with-output-string
                                               (lambda (port)
                                                 (print-exception port #f (exception-kind condition)
                                                          (exception-args condition)))))
                                        output)
                                 (close-port output)))
                              (primitive-exit 1))
                          (lambda ()
                        (setenv "HOME" "/tmp")
                        (setenv "TMPDIR" "/tmp")
                        (setenv "GUIX_DAEMON_SOCKET" socket)
                        (setenv "GUILE_LOAD_PATH" (string-join load-paths ":"))
                        (setenv "GUILE_LOAD_COMPILED_PATH"
                                (string-join compiled-paths ":"))
                        (when directory (chdir directory))
                        (let ((result (guard (condition
                                              (#t
                                               (write
                                                (list 'error
                                                      (call-with-output-string
                                                       (lambda (port)
                                                         (print-exception port #f (exception-kind condition)
                                                          (exception-args condition)))))
                                                output)
                                               (close-port output)
                                               (primitive-exit 1)))
                                        (if evaluate
                                            (evaluate store)
                                            (thunk)))))
                          (write (list 'success result) output)
                          (close-port output)))
                          #:unwind? #t))
                      #:namespaces '(user mnt uts ipc net)
                      #:loopback-network? #f))))
       (close-port output)
       (let ((response (false-if-exception (read input))))
         (close-port input)
         (when (eof-object? response)
           (format (current-error-port)
                   "goonix: pure evaluator exited without a response (status ~s)~%"
                   status))
         (match (and (zero? (exit-status status)) response)
           (('success result) result)
           (('error condition) (error "pure evaluation failed" condition))
           (_ (error "pure evaluator failed" status response))))))))

(define* (load-file-purely store file #:key (kind 'package)
                           (system (%current-system)) target)
  "Evaluate FILE in a fresh Guile process confined to its tracked project.
Only derivation paths cross the process boundary; Scheme objects do not."
  (let* ((file (canonicalize-path file))
         (tree (import-tracked-tree store (dirname file)))
         (mounted-file (string-append tree "/" (basename file)))
         (socket (%daemon-socket-uri))
         (guile %guile)
         (library-root (dirname (dirname (dirname guile))))
         (module-roots
          (filter-map (lambda (path)
                        (and (not (string-prefix? (%store-prefix) path))
                             (not (string-prefix? library-root path))
                             (string-prefix? (getcwd) path)
                             (file-exists? path)
                             path))
                      %load-path))
         (module-trees
          (map (lambda (root)
                 ;; Import only module and compiled-module content, not the
                 ;; entire source checkout.  A bind mount of ROOT would also
                 ;; expose sibling projects and transient test directories.
                 (add-to-store
                  store "pure-eval-guix-modules" #t "sha256" root
                  #:select?
                  (lambda (file stat)
                    (let ((relative (substring file (min (string-length file)
                                                         (string-length root)))))
                      (or (member relative '("/guix" "/gnu"))
                          (string-prefix? "/guix/" relative)
                          (string-prefix? "/gnu/" relative)
                          (string-suffix? ".go" file))))))
               (delete-duplicates module-roots)))
         (captured-load-path (append module-trees
                                    (filter (lambda (path)
                                              (or (string-prefix? (%store-prefix) path)
                                                  (string-prefix? library-root path)))
                                            %load-path)))
         (captured-compiled-path
          (append module-trees
                  (filter (lambda (path)
                            (or (string-prefix? (%store-prefix) path)
                                (string-prefix? library-root path)))
                          %load-compiled-path)))
         (program
          (string-append
           "(setenv \"NIX_STORE_DIR\" \"" (%store-prefix) "\")\n"
           "(use-modules (guix store) (guix packages) (guix gexp) "
           "(guix derivations) (guix monads) (guix profiles) "
           "(ice-9 match) "
           (if (eq? kind 'system)
               "(gnu system) (gnu system image) (gnu image) " "")
           ") "
           "(define s (open-connection)) "
           "(define (evaluate) "
           "(define o (load \"" mounted-file "\")) "
           "(define (lower o) (match o "
           "((? derivation? d) (derivation-file-name d)) "
           "((? package? p) (derivation-file-name (package-derivation s p))) "
           "((? gexp? g) (run-with-store s (gexp->derivation \"pure-eval\" g))) "
           "((? file-like? f) (run-with-store s (lower-object f))) "
           "((? image? i) (run-with-store s (lower-object (system-image i)))) "
           "((? operating-system? o) (run-with-store s (operating-system-derivation o))) "
           "(_ (error \"not a buildable object\")))) "
           "(write (map lower (if (list? o) o (list o))))) "
           "(define (evaluate-system) "
           "(define o (load \"" mounted-file "\")) "
           "(define d (if (image? o) "
           "(run-with-store s (lower-object (system-image o))) "
           "(run-with-store s (operating-system-derivation o)))) "
           "(write (list (derivation-file-name d) (derivation->output-path d)))) "
           "(define (evaluate-environment) "
           "(define o (load \"" mounted-file "\")) "
           "(define m (if (manifest? o) o (packages->manifest "
           "(if (list? o) o (list o))))) "
           "(write (map (lambda (e) (list (manifest-entry-name e) "
           "(manifest-entry-version e) (derivation-file-name "
           "(let ((item (manifest-entry-item e))) "
           "(if (package? item) (package-derivation s item) "
           "(run-with-store s (lower-object item))))) "
           "(manifest-entry-output e))) (manifest-entries m)))) "
           "(parameterize ((%current-system \"" system "\") "
           "(%current-target-system " (if target (string-append "\"" target "\"") "#f")
           ")) (" (case kind
                    ((system) "evaluate-system")
                    ((environment) "evaluate-environment")
                    (else "evaluate")) "))")))
    (match (pipe)
      ((input . output)
       (let ((response #f))
         (let ((status
                (call-with-container
                    (list %writable-/tmp (mapping (%store-prefix))
                          (mapping library-root)
                          (mapping socket))
                  (lambda ()
                    (close-port input)
                    (dup2 (fileno output) 1)
                    (close-port output)
                    (chdir tree)
                    (setenv "HOME" "/tmp")
                    (setenv "TMPDIR" "/tmp")
                    (setenv "GUILE_AUTO_COMPILE" "0")
                    (setenv "GUIX_DAEMON_SOCKET" socket)
                    ;; Only explicitly captured Guix and dependency modules.
                    (let ((environment
                           (list "GUILE_AUTO_COMPILE=0"
                                 (string-append "GUILE_LOAD_PATH="
                                                (string-join captured-load-path ":"))
                                 (string-append "GUILE_LOAD_COMPILED_PATH="
                                                (string-join captured-compiled-path ":"))
                                 "HOME=/tmp" "TMPDIR=/tmp"
                                 (string-append "GUIX_DAEMON_SOCKET=" socket))))
                      (apply execle guile environment guile "--no-auto-compile"
                             "-c" program '())))
                  #:process-spawned-hook
                  (lambda (pid)
                    ;; The parent must not hold the child's write end open:
                    ;; a failed evaluator exits without writing a response,
                    ;; and that must become EOF rather than a permanent read.
                    (close-port output)
                    (set! response (false-if-exception (read input)))))))
           (close-port output)
           (close-port input)
           (unless (and (zero? (exit-status status)) (list? response))
             (error "pure file evaluation failed" status response))
           response))))))
