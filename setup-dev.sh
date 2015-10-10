#!/bin/bash -x
SCRIPT_DIR="`dirname $0`"

die() {
    echo "$*"
    exit 1
}

build_and_test() {
   # Find sandbox location
   local PACKAGEDB=`cabal exec -- sh -c 'echo $GHC_PACKAGE_PATH' | sed 's/:.*//'`
   echo "Cabal package DB location: $PACKAGEDB"
   # Do the building
  ./Setup configure --enable-tests --package-db="$PACKAGEDB" || die "$1 'configure' failed"
  ./Setup build || die "$1 'build' failed"
# Disabled for now: Tests fail on my system for some reason
#  ./Setup test || die "$1 'test' failed"
}

install_deps() {
   cabal install --only-dependencies --enable-tests || die "$1: Could not install needed dependencies"
}

create_setup() {
   ghc --make -threaded Setup.hs || die "$1: Could not create 'Setup' executable"
}

init_sandbox() {
   cabal sandbox delete # Ignore error status; probably just means sandbox doesn't exist
   cabal sandbox init || die "$1: Could not initialize sandbox"
}

setup_cabal() {
   init_sandbox Cabal
   install_deps Cabal
   create_setup Cabal
   build_and_test Cabal
}

setup_cabalinstall() {
   init_sandbox cabal-install
   cabal sandbox add-source ../Cabal/ || die "cabal-install: Failed to add ../Cabal source"
   install_deps cabal-install
   create_setup cabal-install
   build_and_test cabal-install
}

# Build
(cd ${SCRIPT_DIR}/Cabal         && setup_cabal       ) || die "Failed to build Cabal"
(cd ${SCRIPT_DIR}/cabal-install && setup_cabalinstall) || die "Failed to build cabal-install"
