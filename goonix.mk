# goonix: what the fork adds to the build.  Makefile.am includes this file
# once its own lists of modules and tests are complete, so that the upstream
# file carries one hook instead of an entry per file.

MODULES +=					\
  guix/pure-eval.scm				\
  guix/package-overrides.scm			\
  guix/ca.scm					\
  guix/download-cache.scm			\
  guix/ca/scan.scm				\
  guix/scripts/ca.scm				\
  guix/flakes.scm				\
  guix/flakes/syntax.scm			\
  guix/flakes/lock.scm				\
  guix/flakes/options.scm			\
  guix/flakes/checkout.scm			\
  guix/flakes/resolve.scm			\
  guix/flakes/evaluate.scm			\
  guix/flakes/evaluator.scm			\
  guix/flakes/support.scm			\
  guix/scripts/flake.scm			\
  guix/scripts/run.scm

SCM_TESTS +=					\
  tests/ca.scm					\
  tests/ca-build.scm				\
  tests/ca-existing-item.scm			\
  tests/ca-fixed-output.scm			\
  tests/ca-references.scm			\
  tests/ca-rewrite.scm				\
  tests/ca-sharing.scm				\
  tests/download-cache.scm			\
  tests/flakes.scm				\
  tests/flake-eval.scm				\
  tests/flake-actions.scm			\
  tests/goonix-builders.scm			\
  tests/pure-eval.scm				\
  tests/store-path-check.scm

SH_TESTS +=					\
  build-aux/check-goonix-hooks.sh		\
  tests/guix-pure-eval.sh			\
  tests/guix-pure-eval-system.sh		\
  tests/guix-goonix-defaults.sh			\
  tests/guix-flake-shell.sh			\
  tests/guix-flake.sh

# The hook check fails as long as upstream files hold logic of ours or
# unmarked changes.  It is listed here so that the day it passes, the test
# suite says so and this line goes away.
XFAIL_TESTS = build-aux/check-goonix-hooks.sh

EXTRA_DIST +=					\
  build-aux/goonix-hooks.txt			\
  build-aux/goonix-upstream-base		\
  tests/ca-vectors.json				\
  tests/fixtures/flake-shells.scm

if CAN_RUN_TESTS
if BUILD_DAEMON

# The checks of the finaliser that need a store of their own.  Each one is
# given an empty directory.
goonix_ca_store_checks =			\
  inverse-fragment				\
  opaque-utf					\
  fragment-boundary				\
  sibling-order					\
  staged					\
  leftover					\
  final-root					\
  siblings					\
  existing-item					\
  streaming					\
  symlink-modes					\
  path-validation

# The vectors of the token and placeholder formats, the checks of the
# finaliser, then the unit tests of the build records against the test
# suite's own state directory and a 'guix' that runs from the build tree.
check-local:
	$(AM_V_at)$(abs_top_builddir)/nix/ca-test
	$(AM_V_at)directory=`mktemp -d` || exit 1; \
	  $(abs_top_builddir)/nix/ca-test --shared-vectors \
	    $(abs_top_srcdir)/tests/ca-vectors.json "$$directory"; \
	  status=$$?; chmod -R u+w "$$directory"; rm -rf "$$directory"; \
	  test $$status -eq 0 || exit $$status
	$(AM_V_at)for check in $(goonix_ca_store_checks); do		\
	  directory=`mktemp -d` || exit 1;				\
	  $(abs_top_builddir)/nix/ca-test --$$check "$$directory";	\
	  status=$$?;							\
	  chmod -R u+w "$$directory"; rm -rf "$$directory";		\
	  if test $$status -ne 0; then					\
	    echo "nix/ca-test --$$check failed" >&2; exit 1;		\
	  fi;								\
	done
	$(AM_V_at)$(top_builddir)/test-env $(SHELL) -e -c \
	  'helper="$$GUIX_STATE_DIRECTORY/guix-helper"; \
	   printf "#!%s\nexec %s guix \"\$$@\"\n" "$$SHELL" \
	     "$(abs_top_builddir)/pre-inst-env" > "$$helper"; \
	   chmod +x "$$helper"; \
	   $(abs_top_builddir)/nix/records-test "$$GUIX_STATE_DIRECTORY" "$$helper"'

endif BUILD_DAEMON
endif CAN_RUN_TESTS
