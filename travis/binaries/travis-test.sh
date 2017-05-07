#!/bin/sh

. ./travis-common.sh

# --hide-successes uses terminal control characters which mess up
# Travis's log viewer.  So just print them all!
TEST_OPTIONS=""

# Setup symlink so that paths look the same
mkdir -p $(dirname $UPSTREAM_BUILD_DIR)
ln -s $TRAVIS_BUILD_DIR $UPSTREAM_BUILD_DIR

# Run tests
(timed Cabal/unit-tests $TEST_OPTIONS) || exit $?

   if [ "x$PARSEC" = "xYES" ]; then
       # Parser unit tests
       (cd Cabal && timed ./parser-tests $TEST_OPTIONS) || exit $?

       # Test we can parse Hackage
       (cd Cabal && timed ./parser-hackage-tests $TEST_OPTIONS) | tail || exit $?
   fi

if [ "x$CABAL_LIB_ONLY" = "xYES" ]; then
    exit 0;
fi

# ---------------------------------------------------------------------
# cabal-install
# ---------------------------------------------------------------------

# Update index
(timed cabal-install/cabal update) || exit $?

# Run tests
(timed env CABAL_INSTALL_MONOLITHIC_MODE=UnitTests        cabal-install/cabal $TEST_OPTIONS) || exit $?
(timed env CABAL_INSTALL_MONOLITHIC_MODE=MemoryUsageTests cabal-install/cabal $TEST_OPTIONS) || exit $?

# These need the cabal-install directory
(cd cabal-install && timed env CABAL_INSTALL_MONOLITHIC_MODE=SolverQuickCheck  ./cabal $TEST_OPTIONS --quickcheck-tests=1000) || exit $?
(cd cabal-install && timed env CABAL_INSTALL_MONOLITHIC_MODE=IntegrationTests2 ./cabal $TEST_OPTIONS) || exit $?
