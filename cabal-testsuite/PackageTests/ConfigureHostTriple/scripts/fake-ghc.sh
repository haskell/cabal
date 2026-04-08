#!/bin/sh

# A fake GHC that proxies the real GHC but overrides
# "Target platform" to simulate cross-compilation
# to x86_64-w64-mingw32.

case "$*" in
    *--info*)
        ghc "$@" | sed 's/("Target platform","[^"]*")/("Target platform","x86_64-w64-mingw32")/'
        ;;
    *)
        exec ghc "$@"
        ;;
esac
