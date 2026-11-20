# Deliberate differences from the C++ daemon

Every intentional behaviour difference must name the affected item in the
behaviour inventory.  Incidental differences found by differential testing
are defects, not entries here.

The process model is one threaded process rather than one process per client
(H5; inventory 1.1).  The client PID is not written over `argv[1]`
(inventory 1.1 and 9.25).  A vanished client is handled by cancelling its
Haskell thread rather than by the `SIGPOLL` flag (inventory 1.3).

Recursive NAR uploads and hash rewriting are streamed instead of retaining a
whole store item in memory (inventory 9.20).

The optional `build-separate-download-slots` setting gives download jobs their
own `build-max-download-jobs` limit (inventory 5.2). It is off by default:
substitutions and builders share `build-max-jobs`, as in the C++ daemon. The
setting is not enabled in parity comparisons and must not be enabled before
those comparisons pass and the user asks for it.

The build-user pool reads `/etc/passwd` and `/etc/group` directly rather than
calling `getgrnam`, `getpwnam`, and `getgrouplist` (inventory 4.1). This keeps
identity parsing independent of NSS modules in the static/test daemon; the
group file paths can be overridden for the guest tests. The selected group's
GID is used as the builder GID, and its members' other group entries become
supplementary groups. UID 0 and the daemon's own UID are rejected. Each lease
holds the nonblocking `STATE/userpool/UID` lock through the shared file-lock
table, so another daemon process cannot use the same build user. Unrelated
malformed passwd/group lines are ignored; selected identities are validated.
