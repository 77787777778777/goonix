#!/usr/bin/env bash
# goonix: CA-7 file-level substitution gate.  Re-substituting a package after
# a change in one file fetches one file.  Set GOONIX_CA_FILES_PORT to avoid a
# publisher port collision; default 6795.
set -euo pipefail
if ! unshare --user --map-root-user --mount true 2>/dev/null; then
  printf 'SKIP: user/mount namespaces are unavailable\n'
  exit 77
fi

root=$(mktemp -d "${GOONIX_CA_FILES_TMPDIR:-${TMPDIR:-/tmp}}/goonix-ca-files-XXXXXX")
server=$root/server
store=${GOONIX_CA_FILES_STORE:-${root}-store}
port=${GOONIX_CA_FILES_PORT:-6795}
url=http://127.0.0.1:$port
mkdir -p "$store" "$server/db" "$server/etc" "$server/log"
chmod 700 "$server/etc"
cp tests/keys/openpgp/signing-key.sec tests/keys/openpgp/signing-key.pub "$server/etc/"
chmod 400 "$server/etc/signing-key.sec"
printf '#!/bin/sh\nexec /bin/sh "$@"\n' > "$root/shell"
chmod 755 "$root/shell"
export GOONIX_SHELL_SOURCE=$root/shell
export GOONIX_MKDIR_SOURCE=$HOME/.cache/goonix-test-seed/3c68020130d6d813-mkdir

server_pid= publisher_pid=
completed=false
cleanup() {
  for pid in $publisher_pid $server_pid; do
    if test -n "$pid"; then kill "$pid" 2>/dev/null || true; fi
  done
  wait 2>/dev/null || true
  if $completed && test "${GOONIX_CA_FILES_KEEP:-}" != keep; then
    chmod -R u+w "$root" "$store" 2>/dev/null || true
    rm -rf "$root" "$store"
  else
    printf 'retained ca-files fixture: %s\n' "$root" >&2
  fi
}
trap cleanup EXIT INT TERM

export NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$server/db
export GUIX_CONFIGURATION_DIRECTORY=$server/etc GUIX_STATE_DIRECTORY=$server
export GUIX_DAEMON_SOCKET=$server/socket GUIX_LOG_DIRECTORY=$server/log
./pre-inst-env ./guix-daemon --disable-chroot --listen="$server/socket" \
  --substitute-urls= >"$root/server.log" 2>&1 & server_pid=$!
for i in $(seq 1 100); do test -S "$server/socket" && break; sleep .1; done

# Build V1 (three files) and V2 (one file changed) in the server store.
./pre-inst-env guile --no-auto-compile tests/ca-files-fixture.scm "$root/fixture.scm"

NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$server/db \
GUIX_CONFIGURATION_DIRECTORY=$server/etc GUIX_STATE_DIRECTORY=$server \
GUIX_DAEMON_SOCKET=$server/socket \
  ./pre-inst-env guix publish --listen=127.0.0.1 --port="$port" -C0 \
  >"$root/publish.log" 2>&1 & publisher_pid=$!
for i in $(seq 1 100); do
  if curl -fsS "$url/nix-cache-info" >/dev/null; then break; fi
  sleep .1
done

# Client: substitute V1 (all three files fetched), then V2 (one file fetched).
client=$root/client
mkdir -p "$client/store" "$client/db" "$client/etc" "$client/log"
cat > "$root/client-part.sh" <<EOF
set -euo pipefail
export NIX_STORE_DIR=$store GUIX_DATABASE_DIRECTORY=$client/db
export GUIX_CONFIGURATION_DIRECTORY=$client/etc GUIX_STATE_DIRECTORY=$client
export GUIX_DAEMON_SOCKET=$client/socket GUIX_LOG_DIRECTORY=$client/log
export GOONIX_SERVER_URL=$url
./pre-inst-env ./guix-daemon --disable-chroot --listen="\$GUIX_DAEMON_SOCKET" \
  --substitute-urls= >"$root/client-daemon.log" 2>&1 &
daemon_pid=\$!
trap 'kill "\$daemon_pid" 2>/dev/null || true' EXIT
for i in \$(seq 1 100); do test -S "\$GUIX_DAEMON_SOCKET" && break; sleep .1; done
./pre-inst-env guile --no-auto-compile tests/ca-files-client.scm "$root/fixture.scm" "$root"
EOF
unshare --user --map-root-user --mount bash -c \
  "mount --bind '$client/store' '$store' && exec unshare --user --map-user=$(id -u) --map-group=$(id -g) bash '$root/client-part.sh'"

# The gate: V1 fetched three files, V2 fetched exactly one.
grep -F 'V1_FETCHED=3' "$root/result"
grep -F 'V2_FETCHED=1' "$root/result"
# The server saw three /goonix/file/ requests for V1 and one for V2.
v1=$(sed -n 's/.*(v1-name "\([^"]*\)").*/\1/p' "$root/fixture.scm")
v2=$(sed -n 's/.*(v2-name "\([^"]*\)").*/\1/p' "$root/fixture.scm")
test "$(grep -c "^GET /goonix/file/$v1/" "$root/publish.log")" -eq 3
test "$(grep -c "^GET /goonix/file/$v2/" "$root/publish.log")" -eq 1
completed=true
printf 'CA-7 file-level substitution gate: PASS\n'