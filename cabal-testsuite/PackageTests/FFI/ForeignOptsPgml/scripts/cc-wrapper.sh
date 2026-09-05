#!/bin/sh
# Used as the linker driver of GHC's final link:
#
# - in the cabal (v2) test, via the user's explicit -pgml (--ghc-options);
# - in the setup (v1) test, via Cabal's own -pgml: it points GHC's linker at
#   the C compiler resolved with --with-gcc.
#
# GHC drives the link with C compiler style options (-Wl,...), so the
# wrapper delegates to `cc`. It adds -no-pie (GHC does not pass -no-pie to a
# custom linker, see GHC #15319) and -Wl,--wrap=meaning_of_life_pgml, which
# redirects all calls to the "real" function to
# __wrap_meaning_of_life_pgml. Compilation invocations (which carry -c) are
# passed through unchanged.
for arg in "$@"; do
    if [ "$arg" = "-c" ]; then
        exec cc "$@"
    fi
done
exec cc -no-pie -Wl,--wrap=meaning_of_life_pgml "$@"
