#!/usr/bin/env bash
# Check the deliberately small boundary between upstream Guix and goonix.
# New modules are not hooks. Changes to upstream core need a goonix: marker;
# the package and service collections stay available for upstream merges.
set -euo pipefail

base="${GOONIX_UPSTREAM_BASE:-fd49d69fbe0}"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"

failures=0
while IFS= read -r file; do
    [[ -n "$file" ]] || continue
    case "$file" in
        gnu/packages/*|gnu/services/*)
            echo "goonix: upstream merge surface changed: $file" >&2
            failures=1
            ;;
        guix/*|nix/*)
            count="$(git diff --no-ext-diff --unified=0 "$base" -- "$file" |
                grep -Ec '^\+[^+]|^\+$' || true)"
            marked="$(git diff --no-ext-diff --unified=0 "$base" -- "$file" |
                grep -Ec '^\+.*goonix:' || true)"
            if ((count > 0 && marked == 0)); then
                echo "goonix: core edit lacks a goonix: marker: $file" >&2
                failures=1
            fi
            ;;
    esac
done < <(git diff --no-ext-diff --name-only --diff-filter=AM "$base" --)

if ((failures == 0)); then
    echo 'goonix: core hooks are marked; package/service merge surfaces unchanged'
fi
exit "$failures"
