#!/usr/bin/env bash

# A script that will print out $MAGIC_STRING when invoked with the
# '-build-runner' option.
#
# When cabal is invoked with the '--ghcjs' option and the '--with-compiler'
# option set to the path of this script, cabal will successfully get to the linking
# stage (where it *should* call this script with the '-build-runner' option).

MAGIC_STRING="SUCCESS! GHCJS was invoked with '-build-runner' option"

if [ "$1" == "--numeric-ghcjs-version" ]; then
   ghc --numeric-version
elif [ "$1" == "--numeric-ghc-version" ]; then
   ghc --numeric-version
elif [[ "$*" == *-build-runner* ]]; then
   echo "$MAGIC_STRING"
else
   ghc "$@"
fi
