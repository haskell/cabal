#!/bin/sh

# ATTENTION! Before editing this file, maybe you can make a
# separate script to do your test?  We don't want individual
# Travis builds to take too long (they time out at 50min and
# it's generally unpleasant if the build takes that long.)
# If you make a separate matrix entry in .travis.yml it can
# be run in parallel.

. ./travis-common.sh

CABAL_STORE_DB="${HOME}/.cabal/store/ghc-${GHCVER}/package.db"
CABAL_LOCAL_DB="${PWD}/dist-newstyle/packagedb/ghc-${GHCVER}"
CABAL_BDIR="${PWD}/dist-newstyle/build/Cabal-${CABAL_VERSION}"
CABAL_INSTALL_BDIR="${PWD}/dist-newstyle/build/cabal-install-${CABAL_VERSION}"
CABAL_INSTALL_SETUP="${CABAL_INSTALL_BDIR}/setup/setup"
# --hide-successes uses terminal control characters which mess up
# Travis's log viewer.  So just print them all!
TEST_OPTIONS=""

# ---------------------------------------------------------------------
# Update the Cabal index
# ---------------------------------------------------------------------

timed cabal update

# ---------------------------------------------------------------------
# Install happy if necessary
# ---------------------------------------------------------------------

if ! command -v happy; then
    timed cabal install happy
fi

# ---------------------------------------------------------------------
# Setup our local project
# ---------------------------------------------------------------------

cp cabal.project.travis cabal.project.local

# ---------------------------------------------------------------------
# Cabal
# ---------------------------------------------------------------------

# Needed to work around some bugs in nix-local-build code.
export CABAL_BUILDDIR="${CABAL_BDIR}"

# NB: Best to do everything for a single package together as it's
# more efficient (since new-build will uselessly try to rebuild
# Cabal otherwise).
if [ "x$PARSEC" = "xYES" ]; then
  timed cabal new-build -fparsec Cabal Cabal:package-tests Cabal:unit-tests Cabal:parser-tests
else
  timed cabal new-build Cabal Cabal:package-tests Cabal:unit-tests
fi

# NB: the '|| exit $?' workaround is required on old broken versions of bash
# that ship with OS X. See https://github.com/haskell/cabal/pull/3624 and
# http://stackoverflow.com/questions/14970663/why-doesnt-bash-flag-e-exit-when-a-subshell-fails

# Run tests
(export CABAL_PACKAGETESTS_DB_STACK="clear:global:${CABAL_STORE_DB}:${CABAL_LOCAL_DB}"; cd Cabal && timed ${CABAL_BDIR}/build/package-tests/package-tests $TEST_OPTIONS) || exit $?
(cd Cabal && timed ${CABAL_BDIR}/build/unit-tests/unit-tests       $TEST_OPTIONS) || exit $?

if [ "x$PARSEC" = "xYES" ]; then
    (cd Cabal && timed ${CABAL_BDIR}/build/parser-tests/parser-tests $TEST_OPTIONS) | tail || exit $?
fi

# Run haddock (hack: use the Setup script from package-tests!)
(cd Cabal && timed cabal act-as-setup --build-type=Simple -- haddock --builddir=${CABAL_BDIR}) || exit $?

# Redo the package tests with different versions of GHC
if [ "x$TEST_OTHER_VERSIONS" = "xYES" ]; then
    (export CABAL_PACKAGETESTS_WITH_GHC="/opt/ghc/7.0.4/bin/ghc"; \
        cd Cabal && timed ${CABAL_BDIR}/build/package-tests/package-tests $TEST_OPTIONS)
    (export CABAL_PACKAGETESTS_WITH_GHC="/opt/ghc/7.2.2/bin/ghc"; \
        cd Cabal && timed ${CABAL_BDIR}/build/package-tests/package-tests $TEST_OPTIONS)
    (export CABAL_PACKAGETESTS_WITH_GHC="/opt/ghc/head/bin/ghc"; \
        cd Cabal && timed ${CABAL_BDIR}/build/package-tests/package-tests $TEST_OPTIONS)
fi

# Check for package warnings
(cd Cabal && timed cabal check) || exit $?

unset CABAL_BUILDDIR

if [ "x$CABAL_LIB_ONLY" = "xYES" ]; then
    exit 0;
fi

# ---------------------------------------------------------------------
# cabal-install
# ---------------------------------------------------------------------

# Needed to work around some bugs in nix-local-build code.
export CABAL_BUILDDIR="${CABAL_INSTALL_BDIR}"

timed cabal new-build cabal-install:cabal \
                      cabal-install:integration-tests \
                      cabal-install:integration-tests2 \
                      cabal-install:unit-tests \
                      cabal-install:solver-quickcheck

# The integration-tests2 need the hackage index, and need it in the secure
# format, which is not necessarily the default format of the bootstrap cabal.
# If the format does match then this will be very quick.
timed ${CABAL_INSTALL_BDIR}/build/cabal/cabal update

# Run tests
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/unit-tests/unit-tests         $TEST_OPTIONS) || exit $?
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/solver-quickcheck/solver-quickcheck  $TEST_OPTIONS --quickcheck-tests=1000) || exit $?
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/integration-tests/integration-tests  $TEST_OPTIONS) || exit $?
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/integration-tests2/integration-tests2 $TEST_OPTIONS) || exit $?

# Haddock
(cd cabal-install && timed ${CABAL_INSTALL_SETUP} haddock --builddir=${CABAL_INSTALL_BDIR} ) || exit $?

(cd cabal-install && timed cabal check) || exit $?

unset CABAL_BUILDDIR

# Check what we got
${CABAL_INSTALL_BDIR}/build/cabal/cabal --version
