# GNU Guix --- Functional package management for GNU
# Copyright © 2026 the Guix authors
#
# Test evaluation of project files in an isolated environment.

set -e

# Pure evaluation requires mount, UTS and network namespaces.
guile -c '(use-modules (gnu build linux-container))
(exit (if (and (user-namespace-supported?)
               (unprivileged-user-namespace-supported?)
               (setgroups-supported?))
          0 77))'

project="t-pure-eval-project-$$"
outside="t-pure-eval-outside-$$"
trap 'rm -rf "$project" "$outside"' EXIT
mkdir "$project" "$outside"
printf 'outside\n' > "$outside/secret"
git -C "$project" init -q

cat > "$project/environment.scm" <<'EOF'
(use-modules (guix tests))
(dummy-package "pure-eval-environment"
  (version (or (getenv "PURE_EVAL_PROBE")
               (error "environment variable is missing"))))
EOF

cat > "$project/outside.scm" <<EOF
(use-modules (guix tests) (ice-9 rdelim))
(dummy-package "pure-eval-outside"
  (version (call-with-input-file "$PWD/$outside/secret" read-line)))
EOF

cat > "$project/network.scm" <<'EOF'
(use-modules (guix tests))
(dummy-package "pure-eval-network"
  (version (if (getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)
               "0"
               (error "network is unavailable"))))
EOF

cat > "$project/tracked.scm" <<'EOF'
(use-modules (guix tests))
(dummy-package "pure-eval-tracked" (version "0"))
EOF
cat > "$project/untracked.scm" <<'EOF'
(use-modules (guix tests))
(dummy-package "pure-eval-untracked" (version "0"))
EOF
cat > "$project/tracked-project.scm" <<'EOF'
(load "tracked.scm")
EOF
cat > "$project/untracked-project.scm" <<'EOF'
(load "untracked.scm")
EOF
git -C "$project" add environment.scm outside.scm network.scm \
    tracked.scm tracked-project.scm untracked-project.scm

# The controls establish that each probe works during ordinary evaluation.
PURE_EVAL_PROBE=impure guix build -d -f "$project/environment.scm"
guix build -d -f "$project/outside.scm"
guix build -d -f "$project/network.scm"
guix build -d -f "$project/tracked-project.scm"
guix build -d -f "$project/untracked-project.scm"

# No ambient environment, host files, network, or untracked files may leak in.
expect_pure_failure() {
    if "$@"; then
        echo "pure evaluation unexpectedly succeeded: $*" >&2
        exit 1
    fi
}

# The option is independent of its position in the command line.
expect_pure_failure env PURE_EVAL_PROBE=impure \
    guix build -f "$project/environment.scm" --pure-eval -d
expect_pure_failure env PURE_EVAL_PROBE=impure \
    guix build --pure-eval -d -f "$project/environment.scm"
expect_pure_failure guix build --pure-eval -d -f "$project/outside.scm"
expect_pure_failure guix build --pure-eval -d -f "$project/network.scm"
expect_pure_failure guix build --pure-eval -d -f "$project/untracked-project.scm"

# Tracked project files are imported and evaluated successfully.
guix build --pure-eval -d -f "$project/tracked-project.scm"

# Shell reconstruction uses derivations and metadata, not Scheme objects
# crossing the evaluator boundary.  Bootstrap mode avoids a toolchain build.
guix shell --bootstrap --pure-eval -n \
    -f "$project/tracked-project.scm" -- true

exit 0
