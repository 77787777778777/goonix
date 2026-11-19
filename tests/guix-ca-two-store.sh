#!/usr/bin/env bash
# goonix: CA-5 two-store graph sharing gate.  Set GOONIX_TWO_STORE_PORT to
# avoid a publisher port collision; default 6791.
set -euo pipefail

root=$(mktemp -d "${GOONIX_TWO_STORE_TMPDIR:-${TMPDIR:-/tmp}}/goonix-two-store-XXXXXX")
server=$root/server
client=$root/client
store=${GOONIX_TWO_STORE_STORE:-${root}-store}
client_store=$client/store
port=${GOONIX_TWO_STORE_PORT:-6791}
url=http://127.0.0.1:$port
mkdir -p "$store" "$client_store" "$server/db" "$server/etc" "$server/log" \
         "$client/db" "$client/etc" "$client/log"
chmod 700 "$server/etc" "$client/etc"
for side in "$server/etc" "$client/etc"; do
  cp tests/keys/openpgp/signing-key.sec tests/keys/openpgp/signing-key.pub "$side/"
  chmod 400 "$side/signing-key.sec"
done
{
  printf '(acl (entry '
  cat tests/keys/openpgp/signing-key.pub
  printf ' (tag (guix import))))\n'
} > "$client/etc/acl"
printf '#!/bin/sh\nexec /bin/sh "$@"\n' > "$root/shell"
chmod 755 "$root/shell"

server_pid= publisher_pid= client_pid=
completed=false
cleanup() {
  for pid in $publisher_pid $client_pid $server_pid; do
    if test -n "$pid"; then kill "$pid" 2>/dev/null || true; fi
  done
  wait 2>/dev/null || true
  if $completed && test "${GOONIX_TWO_STORE_KEEP:-}" != keep; then
    chmod -R u+w "$root" "$store" 2>/dev/null || true
    rm -rf "$root" "$store"
  else
    printf 'retained two-store fixture: %s\n' "$root" >&2
  fi
}
trap cleanup EXIT INT TERM

export NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$server/db
export GUIX_CONFIGURATION_DIRECTORY=$server/etc GUIX_STATE_DIRECTORY=$server
export GUIX_DAEMON_SOCKET=$server/socket NIX_LOG_DIR=$server/log
export GUIX_ALLOW_UNAUTHENTICATED_SUBSTITUTES=no
export GOONIX_SHELL_SOURCE=$root/shell
export GOONIX_BUILD_MARKER=$root/server-build-marker
./pre-inst-env ./guix-daemon --disable-chroot --listen="$server/socket" \
  --substitute-urls= >"$root/server.log" 2>&1 & server_pid=$!
for i in $(seq 1 100); do test -S "$server/socket" && break; sleep .1; done
./pre-inst-env guile --no-auto-compile tests/ca-two-store-fixture.scm \
  server "$root/fixture.scm"

NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$server/db \
GUIX_CONFIGURATION_DIRECTORY=$server/etc GUIX_STATE_DIRECTORY=$server \
GUIX_DAEMON_SOCKET=$server/socket \
  ./pre-inst-env guix publish --listen=127.0.0.1 --port="$port" -C0 \
  >"$root/publish.log" 2>&1 & publisher_pid=$!
for i in $(seq 1 100); do
  if curl -fsS "$url/nix-cache-info" >/dev/null; then break; fi
  sleep .1
done

cat > "$root/client-part.sh" <<EOF
set -euo pipefail
export NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$client/db
export GUIX_CONFIGURATION_DIRECTORY=$client/etc GUIX_STATE_DIRECTORY=$client
export GUIX_DAEMON_SOCKET=$client/socket NIX_LOG_DIR=$client/log
export GOONIX_SERVER_URL=$url GOONIX_SERVER_STORE=$store
export GOONIX_SHELL_SOURCE=$root/shell GOONIX_BUILD_MARKER=$root/server-build-marker
./pre-inst-env ./guix-daemon --disable-chroot --listen="\$GUIX_DAEMON_SOCKET" \
  --substitute-urls="\$GOONIX_SERVER_URL" >"$root/client-daemon.log" 2>&1 &
daemon_pid=\$!
trap 'kill "\$daemon_pid" 2>/dev/null || true' EXIT
for i in \$(seq 1 100); do test -S "\$GUIX_DAEMON_SOCKET" && break; sleep .1; done
./pre-inst-env guile --no-auto-compile tests/ca-two-store-fixture.scm \
  plain "$root/fixture.scm"
EOF
unshare --user --map-root-user --mount bash -c \
  "mount --bind '$client_store' '$store' && exec unshare --user --map-user=$(id -u) --map-group=$(id -g) bash '$root/client-part.sh'" &
client_pid=$!
wait "$client_pid"

graph_count=$(grep -c '^GET /goonix/records/' "$root/publish.log" || true)
# Two top-level builds: one later derivation and one dependent graph.
test "$graph_count" -eq 2
grep -F "GET /goonix/records/$(sed -n 's/.*(later "\([^"]*\)").*/\1/p' "$root/fixture.scm" | xargs basename)" "$root/publish.log"
grep -F "GET /goonix/records/$(sed -n 's/.*(dependent "\([^"]*\)").*/\1/p' "$root/fixture.scm" | xargs basename)" "$root/publish.log"
test "$(grep -c '^GET /nar/' "$root/publish.log" || true)" -ge 2
cp "$root/publish.log" "$root/plain-publish.log"

# A local result of the same non-reproducible input takes precedence.  The
# dependent must be built against it, not fetched against the server's result.
mkdir -p "$client/two-store" "$client/two-db" "$client/two-state" "$client/two-log"
cat > "$root/two-result-part.sh" <<EOF
set -euo pipefail
export NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$client/two-db
export GUIX_CONFIGURATION_DIRECTORY=$client/etc GUIX_STATE_DIRECTORY=$client/two-state
export GUIX_DAEMON_SOCKET=$client/two.socket NIX_LOG_DIR=$client/two-log
export GOONIX_SERVER_URL=$url GOONIX_SERVER_STORE=$store
export GOONIX_SHELL_SOURCE=$root/shell GOONIX_BUILD_MARKER=$root/server-build-marker
./pre-inst-env ./guix-daemon --disable-chroot --listen="\$GUIX_DAEMON_SOCKET" \
  --substitute-urls="\$GOONIX_SERVER_URL" >"$root/two-daemon.log" 2>&1 &
pid=\$!
trap 'kill "\$pid" 2>/dev/null || true' EXIT
for i in \$(seq 1 100); do test -S "\$GUIX_DAEMON_SOCKET" && break; sleep .1; done
./pre-inst-env guile --no-auto-compile tests/ca-two-store-fixture.scm \
  two-result "$root/fixture.scm"
EOF
before=$(grep -c '^GET /goonix/records/' "$root/publish.log" || true)
unshare --user --map-root-user --mount bash -c \
  "mount --bind '$client/two-store' '$store' && exec unshare --user --map-user=$(id -u) --map-group=$(id -g) bash '$root/two-result-part.sh'"
after=$(grep -c '^GET /goonix/records/' "$root/publish.log" || true)
# Only the first input lookup needs the network.  The locally built input
# and dependent are already named by the imported graph.
test $((after-before)) -eq 1

# Kill only graph helpers.  The ordinary substitute query/download loop must
# still work for another top-level derivation in the same request.
mkdir -p "$client/dead-store" "$client/dead-db" "$client/dead-state" "$client/dead-log"
cat > "$root/dead-part.sh" <<EOF
set -euo pipefail
export NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$client/dead-db
export GUIX_CONFIGURATION_DIRECTORY=$client/etc GUIX_STATE_DIRECTORY=$client/dead-state
export GUIX_DAEMON_SOCKET=$client/dead.socket NIX_LOG_DIR=$client/dead-log
export GOONIX_SERVER_URL=$url GOONIX_SERVER_STORE=$store
export GOONIX_SHELL_SOURCE=$root/shell GOONIX_BUILD_MARKER=$root/server-build-marker
export GOONIX_REAL_GUIX=$PWD/scripts/guix
cat > "$root/dead-guix" <<'INNER'
#!/bin/sh
if test "\$1 \$2" = "substitute --goonix-graph"; then exit 42; fi
exec "\$GOONIX_REAL_GUIX" "\$@"
INNER
chmod 755 "$root/dead-guix"
export GUIX=$root/dead-guix
while IFS= read -r line; do export "\$line"; done < <(
  ./pre-inst-env perl -MEnv -e 'for \$v (qw(GUILE_LOAD_PATH GUILE_LOAD_COMPILED_PATH GOONIX_DEEP_SCAN_HELPER)) { print "\$v=\$ENV{\$v}\\n" }'
)
./guix-daemon --disable-chroot --listen="\$GUIX_DAEMON_SOCKET" \
  --substitute-urls="\$GOONIX_SERVER_URL" --max-jobs=1 \
  >"$root/dead-daemon.log" 2>&1 &
pid=\$!
trap 'kill "\$pid" 2>/dev/null || true' EXIT
for i in \$(seq 1 100); do test -S "\$GUIX_DAEMON_SOCKET" && break; sleep .1; done
./pre-inst-env guile --no-auto-compile tests/ca-two-store-fixture.scm \
  dead "$root/fixture.scm"
EOF
before=$(grep -c '^GET /nar/' "$root/publish.log" || true)
unshare --user --map-root-user --mount bash -c \
  "mount --bind '$client/dead-store' '$store' && exec unshare --user --map-user=$(id -u) --map-group=$(id -g) bash '$root/dead-part.sh'"
after=$(grep -c '^GET /nar/' "$root/publish.log" || true)
test "$after" -gt "$before"
# The client log is the observable build output; the request count and nar
# response prove that graph failure did not disable ordinary substitution.

# A daemon started with --no-substitutes must leave the request log unchanged.
before=$(grep -c '^GET /goonix/records/' "$root/publish.log" || true)
cat > "$root/no-substitutes-part.sh" <<EOF
set -euo pipefail
export NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$client/no-sub-db
export GUIX_CONFIGURATION_DIRECTORY=$client/etc GUIX_STATE_DIRECTORY=$client/no-sub-state
export GUIX_DAEMON_SOCKET=$client/no-substitutes.socket NIX_LOG_DIR=$client/no-sub-log
export GOONIX_SHELL_SOURCE=$root/shell GOONIX_BUILD_MARKER=$root/server-build-marker
mkdir -p "\$GUIX_DATABASE_DIRECTORY" "\$GUIX_STATE_DIRECTORY" "\$NIX_LOG_DIR"
./pre-inst-env ./guix-daemon --disable-chroot --no-substitutes \
  --listen="\$GUIX_DAEMON_SOCKET" >"$root/no-substitutes-daemon.log" 2>&1 &
pid=\$!
trap 'kill "\$pid" 2>/dev/null || true' EXIT
for i in \$(seq 1 100); do test -S "\$GUIX_DAEMON_SOCKET" && break; sleep .1; done
./pre-inst-env guile --no-auto-compile tests/ca-two-store-fixture.scm \
  no-substitutes "$root/fixture.scm"
EOF
mkdir -p "$client/no-sub-store"
unshare --user --map-root-user --mount bash -c \
  "mount --bind '$client/no-sub-store' '$store' && exec unshare --user --map-user=$(id -u) --map-group=$(id -g) bash '$root/no-substitutes-part.sh'"
after=$(grep -c '^GET /goonix/records/' "$root/publish.log" || true)
test "$before" -eq "$after"
completed=true
printf 'two-store graph sharing: PASS\n'
