;;; goonix: public flake data interface.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 goonix contributors
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

(define-module (guix flakes)
  #:use-module (guix flakes syntax)
  #:use-module (guix flakes lock)
  #:use-module (guix flakes resolve)
  #:re-export (flake? flake-format flake-description flake-modules
               flake-inputs flake-options flake-outputs-expression flake-source
               flake-input? flake-input-name flake-input-type flake-input-url
               flake-input-path flake-input-branch flake-input-tag
               flake-input-commit flake-input-as flake-input-worktree?
               flake-input-pinned? flake-input-follows flake-input-overrides
               flake-input-options flake-input-introduction flake-input-same-as
               flake-option? flake-option-name flake-option-type
               flake-option-default flake-option-description
               read-data-file read-data-string read-flake
               flake-lock? make-flake-lock flake-lock flake-lock-version flake-lock-inputs
               locked-input? make-locked-input locked-input locked-input-name
               locked-input-type locked-input-url locked-input-path
               locked-input-branch locked-input-tag locked-input-commit
               locked-input-tree-hash locked-input-as locked-input-worktree?
               locked-input-dependency-path locked-input-identity
               locked-input-authentication locked-input-introduction
               locked-input-edges locked-input-options locked-input-checkout
               lock-edge? make-lock-edge lock-edge lock-edge-from lock-edge-name
               lock-edge-kind lock-edge-target lock-edge-child
               read-flake-lock read-flake-local write-flake-lock
               flake-lock->sexp
               lock-flake update-flake verify-flake-lock
               read-local-overrides effective-flake-lock flake-metadata))
