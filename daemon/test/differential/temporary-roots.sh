#!/usr/bin/env bash
set -euo pipefail
root=$(CDPATH= cd -- "$(dirname "$0")/../../.." && pwd)
. "$root/daemon/test/lib.sh"
out=${1:-$root/.scratch/hs7-temporary-roots}
cxx=${GOONIX_CXX_DAEMON:-$root/.scratch/hs0-cxx-daemon}
hs=${GOONIX_HS_DAEMON:-$root/guix-daemon}
for daemon in "$cxx" "$hs"; do test -x "$daemon"; done

run_one() {
  name=$1 holder_daemon=$2 collector_daemon=$3 fixture=$out/$name
  if test -e "$fixture"; then chmod -R u+rwX "$fixture" 2>/dev/null || true; rm -rf "$fixture"; fi
  mkdir -p "$fixture/store" "$fixture/db" "$fixture/var"
  export NIX_STORE_DIR="$fixture/store" GUIX_DATABASE_DIRECTORY="$fixture/db"
  export GUIX_STATE_DIRECTORY="$fixture/var" GUILE_AUTO_COMPILE=0
  export HS7_FIXTURE="$fixture" HS7_PHASE=hold
  export HS7_HOLDER_SOCKET="$fixture/var/holder.socket"
  export HS7_COLLECTOR_SOCKET="$fixture/var/collector.socket"
  local holder_pid collector_pid holder_client holder_scope collector_scope holder_daemon_scope
  holder_pid= collector_pid= holder_client= holder_daemon_scope= collector_scope= holder_scope=
  cleanup() {
    if test -n "${holder_client:-}"; then goonix_stop_capped "$holder_client" "${holder_scope:-}"; fi
    if test -n "${collector_pid:-}"; then goonix_stop_capped "$collector_pid" "${collector_scope:-}"; fi
    if test -n "${holder_pid:-}"; then goonix_stop_capped "$holder_pid" "${holder_daemon_scope:-}"; fi
    holder_client= collector_pid= holder_pid= holder_scope= collector_scope= holder_daemon_scope=
  }
  trap cleanup EXIT
  goonix_start_capped "$root/pre-inst-env" "$holder_daemon" --disable-chroot --listen="$HS7_HOLDER_SOCKET" >"$fixture/holder-daemon.log" 2>&1
  holder_pid=$GOONIX_TEST_PID holder_daemon_scope=$GOONIX_TEST_SCOPE
  for i in $(seq 1 300); do test -S "$HS7_HOLDER_SOCKET" && break; kill -0 "$holder_pid"; sleep .01; done
  test -S "$HS7_HOLDER_SOCKET"
  goonix_start_capped "$root/pre-inst-env" "$collector_daemon" --disable-chroot --listen="$HS7_COLLECTOR_SOCKET" >"$fixture/collector-daemon.log" 2>&1
  collector_pid=$GOONIX_TEST_PID collector_scope=$GOONIX_TEST_SCOPE
  for i in $(seq 1 300); do test -S "$HS7_COLLECTOR_SOCKET" && break; kill -0 "$collector_pid"; sleep .01; done
  test -S "$HS7_COLLECTOR_SOCKET"
  goonix_start_capped "$root/pre-inst-env" guile --no-auto-compile "$root/daemon/test/differential/temporary-root-holder.scm" >"$fixture/holder-client.log" 2>&1
  holder_client=$GOONIX_TEST_PID holder_scope=$GOONIX_TEST_SCOPE
  for i in $(seq 1 300); do test -s "$fixture/holder-ready" && break; kill -0 "$holder_client"; sleep .01; done
  test -s "$fixture/holder-ready"
  test "$(find "$fixture/var/temproots" -mindepth 1 -maxdepth 1 -type f | wc -l)" -eq 1
  test ! -e "$fixture/var/temproots/goonix"
  HS7_PHASE=collect goonix_run_capped "$root/pre-inst-env" guile --no-auto-compile "$root/daemon/test/differential/temporary-root-collect.scm"
  printf 'HS-7 concurrent temporary roots %s: PASS\n' "$name"
}
mkdir -p "$out"
run_one cxx-hs "$cxx" "$hs"
run_one hs-cxx "$hs" "$cxx"
run_one cxx-cxx "$cxx" "$cxx"
