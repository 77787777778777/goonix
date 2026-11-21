;;; goonix: host side of content-addressed grafts.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix ca graft)
  #:use-module (guix derivations)
  #:export (%graft-content-addressed?
            goonix-graft-modules
            goonix-graft-procedure
            goonix-graft-load
            goonix-graft-addressing))

;;; Commentary:
;;;
;;; A graft of a content-addressed derivation writes its own outputs at
;;; self-tokens (the reserved "eout" alphabet).  Upstream's graft builder
;;; cannot rewrite those: its rewriter only accepts nix-base32 hashes.  Such
;;; a graft therefore imports (guix ca build graft) and calls
;;; graft/content-addressed, which applies the graft through upstream's
;;; rewriter in two passes.
;;;
;;; The three helpers below are substituted into upstream's build gexp in
;;; guix/grafts.scm (marked goonix:).  For an input-addressed DRV they return
;;; the module list unchanged, the plain 'graft' symbol and an empty load
;;; list, so the builder text and the module list — and hence the derivation
;;; — are exactly upstream's.  This module lives outside guix/build so that
;;; no upstream build module imports it.
;;;
;;; Code:

(define %graft-content-addressed?
  ;; Whether grafts are forced content-addressed even over an input-addressed
  ;; derivation.  Adoption (guix ca adopt) sets this: an adopted item is a
  ;; content-addressed graft of an upstream input-addressed binary.  Off by
  ;; default, so an ordinary input-addressed graft stays exactly upstream's.
  (make-parameter #f))

(define (content-addressed-graft? drv)
  "Whether grafting DRV writes content-addressing self-tokens.  Either DRV is
not input-addressed — it carries a goonixAddressing marker — so the graft
derivation over it is content-addressed; or %graft-content-addressed? forces
it (adoption).  (The marker, not the output path, is the test: on the host a
content-addressed derivation's output path is a placeholder, and the
self-token only appears in the builder's environment.)"
  (or (%graft-content-addressed?)
      (and (assoc-ref (derivation-builder-environment-vars drv)
                      "goonixAddressing")
           #t)))

(define (goonix-graft-modules drv modules)
  "Return MODULES, the import list of a graft builder for DRV: unchanged for
an input-addressed DRV, with (guix ca build graft) appended for a
content-addressed one."
  (if (content-addressed-graft? drv)
      (append modules '((guix ca build graft)))
      modules))

(define (goonix-graft-procedure drv)
  "Return the graft procedure a graft builder for DRV calls: the plain
'graft' symbol for an input-addressed DRV (so the gexp text is upstream's),
'graft/content-addressed' for a content-addressed one."
  (if (content-addressed-graft? drv)
      'graft/content-addressed
      'graft))

(define (goonix-graft-load drv)
  "Return the list of forms that load the content-addressed graft module
into a graft builder for DRV: the empty list for an input-addressed DRV (so
the builder text is exactly upstream's — spliced with #+@, an empty list adds
no form), a use-modules form for a content-addressed one.  The module is
imported by goonix-graft-modules but must also be loaded before
graft/content-addressed is called."
  (if (content-addressed-graft? drv)
      '((use-modules (guix ca build graft)))
      '()))

(define (goonix-graft-addressing drv)
  "Return the #:addressing argument for a graft derivation over DRV: 'content
when the graft writes self-tokens (DRV is content-addressed, or adoption
forces it), otherwise #f so the graft's addressing is decided as upstream's
is.  Returning #f keeps an input-addressed graft's derivation upstream's."
  (and (content-addressed-graft? drv) 'content))

;;; graft.scm ends here