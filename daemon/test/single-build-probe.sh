#!/usr/bin/env bash
set -euo pipefail
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
. "$root/daemon/test/lib.sh"
daemon=${1:?daemon}
out=${2:-$root/.scratch/single-build}
mkdir -p "$out"
fixture=$(mktemp -d /tmp/goonix-single-build-XXXXXX)
pid= scope=
cleanup() { status=$?; trap - EXIT; test -n "$pid" && goonix_stop_capped "$pid" "$scope"; rm -rf "$fixture"; return "$status"; }
trap cleanup EXIT
export NIX_STORE_DIR="$fixture/store" GUIX_DATABASE_DIRECTORY="$fixture/db" GUIX_STATE_DIRECTORY="$fixture/var" GUIX_DAEMON_SOCKET="$fixture/s" GUILE_AUTO_COMPILE=0
mkdir -p "$NIX_STORE_DIR" "$GUIX_DATABASE_DIRECTORY" "$GUIX_STATE_DIRECTORY"
goonix_start_capped "$root/pre-inst-env" "$daemon" --disable-chroot --no-substitutes --listen="$GUIX_DAEMON_SOCKET" >"$out/daemon.log" 2>&1
pid=$GOONIX_TEST_PID scope=$GOONIX_TEST_SCOPE
for i in $(seq 1 300); do test -S "$GUIX_DAEMON_SOCKET" && break; kill -0 "$pid"; sleep .01; done
goonix_run_capped "$root/pre-inst-env" guile --no-auto-compile "$root/.scratch/single-build-probe.scm" >"$out/client.log" 2>&1
cat "$out/client.log"
