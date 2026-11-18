#!/bin/sh
set -eu

if test "$#" -ne 1; then
    echo "usage: run.sh GHC" >&2
    exit 2
fi

case "$1" in
    */*) ghc=$1 ;;
    *) ghc=$(command -v "$1") ;;
esac

test -x "$ghc" || {
    echo "not an executable compiler: $1" >&2
    exit 2
}

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
daemon=$(CDPATH= cd -- "$here/../.." && pwd)
build="$here/.build"
mkdir -p "$build"

"$ghc" -fforce-recomp -Wall -Wcompat -Widentities \
    -Wincomplete-record-updates -Wincomplete-uni-patterns \
    -Wredundant-constraints -Wmissing-export-lists \
    -threaded -with-rtsopts=-N -package network -i"$daemon/src" -outputdir "$build/proxy" \
    -o "$build/wire-proxy" "$daemon/proxy/Main.hs"

"$ghc" -fforce-recomp -Wall -Wcompat -Widentities \
    -Wincomplete-record-updates -Wincomplete-uni-patterns \
    -Wredundant-constraints -Wmissing-export-lists \
    -i"$daemon/src" -outputdir "$build/tests" \
    -o "$build/wire-proxy-test" "$here/Main.hs"

"$build/wire-proxy-test" "$build/wire-proxy"
