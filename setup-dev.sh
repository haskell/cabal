#!/bin/bash -x
SCRIPT_DIR="`dirname $0`"

die() {
    echo "$*"
    exit 1
}

setup() {
    # Extract parameters
    local NAME="$1"
    shift
    local DEPS="$@"
    # (Re-)create sandbox
    cabal sandbox delete # Ignore error status; probably just means sandbox doesn't exist
    cabal sandbox init || die "$NAME: Could not initialize sandbox"
    # Add dependencies
    for DEP in $DEPS; do
        cabal sandbox add-source "$DEP"
    done
    # Install dependencies
    cabal install --only-dependencies --enable-tests || die "$NAME: Could not install needed dependencies"
    # Build the 'Setup' executable
    ghc --make -threaded -i -i. Setup.hs || die "$NAME: Could not create 'Setup' executable"
    # Build the package
    local PACKAGEDB=`cabal exec -- sh -c 'echo $GHC_PACKAGE_PATH' | sed 's/:.*//'`
    echo "Cabal package DB location: $PACKAGEDB"
    ./Setup configure --enable-tests --package-db="$PACKAGEDB" || die "$NAME: 'configure' failed"
    ./Setup build || die "$NAME: 'build' failed"
}

# Build
(cd ${SCRIPT_DIR}/Cabal         && setup "Cabal"                 ) || die "Failed to build Cabal"
(cd ${SCRIPT_DIR}/cabal-install && setup "cabal-install" ../Cabal) || die "Failed to build cabal-install"
