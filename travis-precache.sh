#!/bin/sh

. ./travis-common.sh

# ---------------------------------------------------------------------
# Update the Cabal index
# ---------------------------------------------------------------------

timed cabal update

# ---------------------------------------------------------------------
# Setup our local project
# ---------------------------------------------------------------------

cp cabal.project.travis cabal.project.local

# ---------------------------------------------------------------------
# Install executables if necessary
# ---------------------------------------------------------------------

if ! command -v happy; then
    timed cabal install $jobs happy
fi

# ---------------------------------------------------------------------
# Warm up the cache
# ---------------------------------------------------------------------

# TODO: Don't build Cabal and hackage-security here, they'll be
# rebuilt anyway.
if [ "x$CABAL_LIB_ONLY" = "xYES" ]; then
    timed cabal new-build --only-dependencies --enable-tests Cabal
else
    timed cabal new-build --only-dependencies --enable-tests cabal-install cabal-tests
fi
