# GNU Guix --- Functional package management for GNU
#
# This file is part of GNU Guix.
#
# GNU Guix is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

if BUILD_HASKELL_DAEMON

bin_PROGRAMS =
noinst_LIBRARIES =
BUILT_SOURCES += daemon/src/Goonix/Config.hs
CLEANFILES += daemon/src/Goonix/Config.hs guix-daemon

# Keep the direct build independent of Cabal's package database and of Hackage.
DAEMON_GHC_FLAGS = -O2 -Wall -Wcompat -Widentities -Wredundant-constraints \
  -threaded -with-rtsopts=-N \
  -XGHC2024 -XStrictData -i$(top_srcdir)/daemon/src \
  -i$(top_builddir)/daemon/src $(LIBGCRYPT_CPPFLAGS) \
  $(LIBGCRYPT_LDFLAGS) $(LIBGCRYPT_LIBS)

# All Haskell modules are prerequisites through the generated dependency file.
-include daemon/.depends.mk

daemon/.depends.mk:
	$(AM_V_GEN)$(MKDIR_P) daemon; \
	$(GHC) -M -dep-makefile=$@.tmp $(DAEMON_GHC_FLAGS) \
	  $(top_srcdir)/daemon/app/Main.hs; mv $@.tmp $@

guix-daemon: daemon/src/Goonix/Config.hs daemon/.depends.mk
	$(AM_V_GEN)$(GHC) --make $(DAEMON_GHC_FLAGS) \
	  -outputdir daemon/ghc-build -o $@.tmp \
	  $(top_srcdir)/daemon/app/Main.hs; mv $@.tmp $@

install-exec-local: guix-daemon
	$(MKDIR_P) "$(DESTDIR)$(bindir)"
	$(INSTALL_PROGRAM) guix-daemon "$(DESTDIR)$(bindir)/guix-daemon"

uninstall-local: uninstall-haskell-daemon
uninstall-haskell-daemon:
	rm -f "$(DESTDIR)$(bindir)/guix-daemon"

daemon/src/Goonix/Config.hs: Makefile guix/store/schema.sql
	$(AM_V_GEN)$(MKDIR_P) daemon/src/Goonix; \
	$(GUILE) --no-auto-compile $(top_srcdir)/daemon/make-config.scm \
	  "$@" "$(storedir)" "$(localstatedir)/guix" \
	  "$(localstatedir)/log/guix" "$(sysconfdir)/guix" "$(bindir)" \
	  "$(localedir)" "$(guix_system)" "$(GUIX_SUBSTITUTE_URLS)" \
	  "$(top_srcdir)/guix/store/schema.sql"

else !BUILD_HASKELL_DAEMON
# The Haskell build owns a different target graph.  Force the existing C++
# target to relink when configure switches back, even if a stub is newer.
.PHONY: guix-daemon
endif !BUILD_HASKELL_DAEMON

EXTRA_DIST += \
  daemon/cabal.project daemon/guix-daemon.cabal daemon/local.mk \
  daemon/make-config.scm daemon/app/Main.hs \
  daemon/src/Goonix/Store/Wire.hs \
  daemon/src/Goonix/Store/Base32.hs daemon/src/Goonix/Store/Hash.hs \
  daemon/src/Goonix/Store/Path.hs daemon/src/Goonix/Store/Derivation.hs \
  daemon/src/Goonix/Store/Nar.hs daemon/src/Goonix/Store/References.hs \
  daemon/proxy/Main.hs \
  daemon/test/wire-proxy/Main.hs daemon/test/wire-proxy/run.sh \
  daemon/test/wire-proxy/inventory.scm daemon/test/check-build-flags.scm \
  daemon/test/formats/Main.hs daemon/test/properties/Main.hs \
  daemon/sandbox/README.md daemon/sandbox/guix-sandbox.cc \
  daemon/sandbox/Makefile \
  daemon/test/sandbox-probe/README.md \
  daemon/test/sandbox-probe/UPSTREAM-REUSE.md \
  daemon/test/sandbox-probe/ThreadedUnshareProbe.hs \
  daemon/test/sandbox-probe/child-inspect.c \
  daemon/test/sandbox-probe/gate.sh daemon/test/sandbox-probe/spawn.c \
  daemon/test/vectors/README.md \
  daemon/test/vectors/Generate.hs daemon/test/vectors/dump-nar.cc \
  daemon/test/vectors/hash-nar.cc daemon/test/vectors/inspect-item.cc \
  daemon/test/vectors/hash-derivations.cc \
  daemon/test/vectors/generate.sh daemon/test/compare-trs.sh \
  daemon/test/RESULTS.md \
  daemon/test/transcripts/README.md \
  daemon/test/transcripts/inventory.wire.log \
  daemon/test/transcripts/inventory-client.log

.PHONY: check-daemon-differential check-haskell-daemon-build check-cxx-daemon-build check-daemon-build-flags
check-daemon-build-flags:
	$(GUILE) --no-auto-compile $(top_srcdir)/daemon/test/check-build-flags.scm \
	  $(top_srcdir)/daemon/guix-daemon.cabal $(top_srcdir)/daemon/local.mk

check-cxx-daemon-build: check-daemon-build-flags
	./config.status --recheck
	./configure --disable-haskell-daemon
	$(MAKE) -j1 guix-daemon

check-haskell-daemon-build: check-daemon-build-flags
	./config.status --recheck
	./configure --with-ghc="$(GHC)"
	$(MAKE) -j1 guix-daemon

check-daemon-differential:
	$(AM_V_GEN)$(GHC) --make -XGHC2024 -XStrictData \
	  -i$(top_srcdir)/daemon/src -i$(top_srcdir)/daemon/test/differential \
	  $(top_srcdir)/daemon/test/differential/Main.hs \
	  -o daemon/check-differential && daemon/check-differential
