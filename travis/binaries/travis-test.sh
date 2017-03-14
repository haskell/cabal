#!/bin/sh

. ./travis-common.sh

CABAL_STORE_DB="${HOME}/.cabal/store/ghc-${GHCVER}/package.db"
CABAL_LOCAL_DB="${PWD}/dist-newstyle/packagedb/ghc-${GHCVER}"
CABAL_BDIR="${PWD}/dist-newstyle/build/Cabal-${CABAL_VERSION}"
CABAL_TESTSUITE_BDIR="${PWD}/dist-newstyle/build/cabal-testsuite-${CABAL_VERSION}"
CABAL_INSTALL_BDIR="${PWD}/dist-newstyle/build/cabal-install-${CABAL_VERSION}"
CABAL_INSTALL_SETUP="${CABAL_INSTALL_BDIR}/setup/setup"
HACKAGE_REPO_TOOL_BDIR="${PWD}/dist-newstyle/build/hackage-repo-tool-${HACKAGE_REPO_TOOL_VERSION}"
# --hide-successes uses terminal control characters which mess up
# Travis's log viewer.  So just print them all!
TEST_OPTIONS=""

# Setup symlink so that paths look the same
mkdir -p $(dirname $UPSTREAM_BUILD_DIR)
ln -s $TRAVIS_BUILD_DIR $UPSTREAM_BUILD_DIR

# Touch package database cache files, so we don't complain they're
# stale (Git doesn't preserve modification times, so we'll end
# up with something wrong.)
touch "$CABAL_STORE_DB/package.cache"
touch "$CABAL_LOCAL_DB/package.cache"

# Run tests
(timed ${CABAL_BDIR}/build/unit-tests/unit-tests $TEST_OPTIONS) || exit $?

   if [ "x$PARSEC" = "xYES" ]; then
       # Parser unit tests
       (cd Cabal && timed ${CABAL_BDIR}/build/parser-tests/parser-tests $TEST_OPTIONS) || exit $?

       # Test we can parse Hackage
       (cd Cabal && timed ${CABAL_BDIR}/build/parser-tests/parser-hackage-tests $TEST_OPTIONS) | tail || exit $?
   fi

(cd cabal-testsuite && timed ${CABAL_TESTSUITE_BDIR}/build/cabal-tests/cabal-tests --builddir=${CABAL_TESTSUITE_BDIR} -j3 $TEST_OPTIONS) || exit $?

# Redo the package tests with different versions of GHC
if [ "x$TEST_OTHER_VERSIONS" = "xYES" ]; then
    (cd cabal-testsuite && timed ${CABAL_TESTSUITE_BDIR}/build/cabal-tests/cabal-tests --builddir=${CABAL_TESTSUITE_BDIR} $TEST_OPTIONS --with-ghc="/opt/ghc/7.0.4/bin/ghc")
    (cd cabal-testsuite && timed ${CABAL_TESTSUITE_BDIR}/build/cabal-tests/cabal-tests --builddir=${CABAL_TESTSUITE_BDIR} $TEST_OPTIONS --with-ghc="/opt/ghc/7.2.2/bin/ghc")
    (cd cabal-testsuite && timed ${CABAL_TESTSUITE_BDIR}/build/cabal-tests/cabal-tests --builddir=${CABAL_TESTSUITE_BDIR} $TEST_OPTIONS --with-ghc="/opt/ghc/head/bin/ghc")
fi

if [ "x$CABAL_LIB_ONLY" = "xYES" ]; then
    exit 0;
fi

# ---------------------------------------------------------------------
# cabal-install
# ---------------------------------------------------------------------

# Update index
(timed ${CABAL_INSTALL_BDIR}/build/cabal/cabal update) || exit $?

# Run tests
(timed ${CABAL_INSTALL_BDIR}/build/unit-tests/unit-tests         $TEST_OPTIONS) || exit $?
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/solver-quickcheck/solver-quickcheck  $TEST_OPTIONS --quickcheck-tests=1000) || exit $?
(timed ${CABAL_INSTALL_BDIR}/build/memory-usage-tests/memory-usage-tests $TEST_OPTIONS) || exit $?

# These need the cabal-install directory
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/integration-tests2/integration-tests2 $TEST_OPTIONS) || exit $?

# Big tests
(cd cabal-testsuite && timed ${CABAL_TESTSUITE_BDIR}/build/cabal-tests/cabal-tests --builddir=${CABAL_TESTSUITE_BDIR} -j3 --skip-setup-tests --with-cabal ${CABAL_INSTALL_BDIR}/build/cabal/cabal --with-hackage-repo-tool ${HACKAGE_REPO_TOOL_BDIR}/build/hackage-repo-tool/hackage-repo-tool $TEST_OPTIONS) || exit $?
