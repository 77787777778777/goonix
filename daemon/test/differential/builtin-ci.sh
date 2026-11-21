#!/usr/bin/env bash
# goonix: builtin download/git-download under a CI-like daemon (HS-8 switch-2).
# The CI runner starts the daemon with `systemd-run --setenv=TMPDIR=…
# --setenv=LC_ALL=…`, so there is no GUIX and no guix on PATH.  The daemon must
# resolve the guix program via NIX_BIN_DIR / the configured bindir, never PATH.
# Each daemon is started with `env -i TMPDIR=… LC_ALL=C.UTF-8 PATH=/nonexistent`
# (no GUIX; NIX_BIN_DIR points at the real guix bindir, as the configured bindir
# would).  A client builds builtin:download + builtin:git-download and checks
# that a failing builtin build leaves a log `guix build --log-file' finds.
# Calibrated: 2caf36817f4's Haskell daemon (literal "guix" PATH fallback) fails.
set -u
root=$(CDPATH= cd -- "$(dirname "$0")/../../.." && pwd)
cxx=${GOONIX_CXX_DAEMON:-$root/.scratch/hs0-cxx-daemon}
hs=${GOONIX_HS_DAEMON:-$root/guix-daemon}
out=${1:-/tmp/gx-builtin-ci}
for daemon in "$cxx" "$hs"; do test -x "$daemon"; done
rm -rf "$out"; mkdir -p "$out"
guixbin=$(dirname "$(command -v guix)")
test -x "$guixbin/guix" || { echo "no guix on the test host PATH" >&2; exit 2; }

run_one() {
  name=$1 daemon=$2
  base=$out/$name
  mkdir -p "$base/store" "$base/db" "$base/var" "$base/log" "$base/tmp"
  sqlite3 "$base/db/db.sqlite" < "$root/guix/store/schema.sql"
  # CI-like environment: no GUIX, no guix on PATH.  NIX_BIN_DIR is the resolved
  # bindir the fixed daemon uses; the old daemon ignores it and searches PATH.
  env -i TMPDIR="$base/tmp" LC_ALL=C.UTF-8 PATH=/nonexistent NIX_BIN_DIR="$guixbin" \
    NIX_STORE_DIR="$base/store" GUIX_DATABASE_DIRECTORY="$base/db" \
    GUIX_STATE_DIRECTORY="$base/var" GUIX_LOG_DIRECTORY="$base/log" \
    GUIX_DAEMON_SOCKET="$base/var/socket" \
    "$daemon" --disable-chroot --listen="$base/var/socket" >"$base/daemon.log" 2>&1 &
  pid=$!
  for i in $(seq 1 300); do test -S "$base/var/socket" && break; kill -0 "$pid" 2>/dev/null || break; sleep .05; done
  if ! test -S "$base/var/socket"; then
    echo "FAIL: $name: daemon did not start"; tail -3 "$base/daemon.log" >&2
    return 1
  fi
  # The client has guix on PATH (pre-inst-env); it talks to the CI-like daemon.
  ( cd "$root" && GUIX_DAEMON_SOCKET="$base/var/socket" NIX_STORE_DIR="$base/store" \
    GUIX_DATABASE_DIRECTORY="$base/db" GUIX_STATE_DIRECTORY="$base/var" \
    GUIX_LOG_DIRECTORY="$base/log" XDG_CACHE_HOME="$base/cache" \
    ./pre-inst-env guile --no-auto-compile "$root/daemon/test/differential/builtin-ci.scm" ) \
    >"$base/client.log" 2>&1
  rc=$?
  kill "$pid" 2>/dev/null; wait "$pid" 2>/dev/null
  chmod -R u+rwX "$base" 2>/dev/null
  if test "$rc" -eq 0; then
    echo "PASS: $name ($(grep -o 'download-ok=[^ ]* git-download-ok=[^ ]* failing-log-found=[^ ]*' "$base/client.log" | head -1))"
  else
    echo "FAIL: $name"
    grep -E 'download-ok=|ERROR|error|failed|Unbound' "$base/client.log" | head -5 >&2
  fi
  return "$rc"
}

status=0
run_one cxx "$cxx" || status=1
run_one hs "$hs" || status=1
if test "$status" -eq 0; then echo "builtin CI differential: PASS"; else echo "builtin CI differential: FAIL" >&2; fi
exit "$status"