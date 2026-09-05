#!/bin/sh
# Emulates a C compiler driver without -no-pie support (like GCC before
# version 6) whose default output is not PIE, so linking GHC's objects
# works without the flag: it fails if it ever sees -no-pie (which is the
# shape of Cabal's support probe), and asks the linker itself for non-PIE
# output instead. Used by the ForeignOptsPgml test to assert that Cabal
# probes the resolved C compiler and does not pass -pgml-supports-no-pie
# to GHC when the probe fails.
for arg in "$@"; do
    if [ "$arg" = "-no-pie" ]; then
        echo "unsupported option -no-pie" >&2
        exit 1
    fi
done
exec cc -Wl,--no-pie "$@"
