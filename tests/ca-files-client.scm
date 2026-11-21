;;; goonix: CA-7 file-level substitution client.  Substitutes V1 (three files
;;; fetched) then V2 (one file changed, one fetched) from the server's
;;; /goonix/files and /goonix/file endpoints, using the client store's .links.
(use-modules (guix) (guix tests) (guix store) (guix ca files)
             ((guix build utils) #:select (mkdir-p))
             (ice-9 match) (ice-9 ftw) (ice-9 textual-ports) (srfi srfi-1))

(define store (open-connection))
(define cache-url (getenv "GOONIX_SERVER_URL"))
(match (command-line)
  ((_ fixture-file root)
   (let* ((data (call-with-input-file fixture-file read))
          (field (lambda (name) (cadr (assq name data))))
          (v1-name (field 'v1-name))
          (v2-name (field 'v2-name))
          (store-dir (%store-prefix))
          (links-dir (string-append store-dir "/.links"))
          (v1-root (string-append root "/v1"))
          (v2-root (string-append root "/v2")))
     (mkdir-p v1-root)
     (mkdir-p v2-root)
     ;; Substitute V1: nothing in .links yet, all three files fetched.
     (let ((n1 (substitute-files cache-url v1-name v1-root
                                 #:links-dir links-dir)))
       ;; Populate .links from the fetched V1 (as the daemon's dedup would).
       ;; Copy rather than hard-link: the assembly root and the store may be
       ;; on different filesystems.
       (for-each (match-lambda
                   ((path type hash size)
                    (let ((lp (string-append links-dir "/" hash)))
                      (unless (file-exists? lp)
                        (copy-file (string-append v1-root "/" path) lp)))))
                 (store-item-files v1-root))
       ;; Substitute V2: only "b" changed, so only "b" is fetched.
       (let ((n2 (substitute-files cache-url v2-name v2-root
                                   #:links-dir links-dir)))
         (call-with-output-file (string-append root "/result")
           (lambda (port)
             (format port "V1_FETCHED=~a~%" n1)
             (format port "V2_FETCHED=~a~%" n2)))
         ;; Sanity: the changed content is present in V2.
         (unless (string=? "BETA-CHANGED"
                           (call-with-input-file (string-append v2-root "/b")
                             get-string-all))
           (error "V2 file b content wrong")))))))
(close-connection store)