#!/usr/bin/env bash
set -euo pipefail
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
. "$root/daemon/test/lib.sh"
daemon=${GOONIX_HS_DAEMON:?}
fixture=${1:?fixture}
mkdir -p "$fixture"
export NIX_STORE_DIR="$fixture/store" GUIX_DATABASE_DIRECTORY="$fixture/db" GUIX_STATE_DIRECTORY="$fixture/var" GUIX_DAEMON_SOCKET="$fixture/s" GUILE_AUTO_COMPILE=0 HS4_SCHEDULER_FIXTURE="$fixture"
mkdir -p "$NIX_STORE_DIR" "$GUIX_DATABASE_DIRECTORY" "$GUIX_STATE_DIRECTORY"
mkfifo "$fixture/tick"
exec 9<>"$fixture/tick"
pid= scope=
cleanup() { status=$?; trap - EXIT; test -e "$fixture/release-waiting" || touch "$fixture/release-waiting"; test -n "$pid" && goonix_stop_capped "$pid" "$scope"; return "$status"; }
trap cleanup EXIT
goonix_start_capped "$root/pre-inst-env" "$daemon" --disable-chroot --no-substitutes --listen="$GUIX_DAEMON_SOCKET" >"$fixture/daemon.log" 2>&1
pid=$GOONIX_TEST_PID scope=$GOONIX_TEST_SCOPE
for i in $(seq 1 300); do test -S "$GUIX_DAEMON_SOCKET" && break; kill -0 "$pid"; sleep .01; done
goonix_run_capped "$root/pre-inst-env" guile --no-auto-compile -c '(use-modules (guix store) (guix derivations) (guix tests) (gnu packages bootstrap) (srfi srfi-34)) (%graft? #f) (let* ((s (open-connection (getenv "GUIX_DAEMON_SOCKET"))) (sh (add-to-store s "bash" #t "sha256" (search-bootstrap-binary "bash" "x86_64-linux"))) (failbody (add-text-to-store s "failbody" "exit 1\n")) (waitbody (add-text-to-store s "waitbody" (string-append "while ! test -e \"" (getenv "HS4_SCHEDULER_FIXTURE") "/release-waiting\"; do read -t 1 _ < \"" (getenv "HS4_SCHEDULER_FIXTURE") "/tick\" || true; done\n"))) (f (derivation s "fail" sh (list "-e" failbody) #:sources (list sh failbody))) (w (derivation s "wait" sh (list "-e" waitbody) #:sources (list sh waitbody)))) (set-build-options s #:use-substitutes? #f #:offload? #f #:max-build-jobs 2) (guard (c ((store-protocol-error? c) (format #t "expected failure: ~a~%" (store-protocol-error-message c)))) (build-derivations s (list f w)) (error "missing failure")))' >"$fixture/client.log" 2>&1
cat "$fixture/client.log"
