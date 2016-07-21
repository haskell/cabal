#!/bin/sh

# ATTENTION! Before editing this file, maybe you can make a
# separate script to do your test?  We don't want individual
# Travis builds to take too long (they time out at 50min and
# it's generally unpleasant if the build takes that long.)
# If you make a separate matrix entry in .travis.yml it can
# be run in parallel.

. ./travis-common.sh

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

export CABAL_BUILDDIR="${CABAL_BDIR}"

# NB: Best to do everything for a single package together as it's
# more efficient (since new-build will uselessly try to rebuild
# Cabal otherwise).
timed cabal new-build Cabal Cabal:package-tests Cabal:unit-tests

# Run tests
(cd Cabal && timed ${CABAL_BDIR}/build/package-tests/package-tests $TEST_OPTIONS)
(cd Cabal && timed ${CABAL_BDIR}/build/unit-tests/unit-tests       $TEST_OPTIONS)

# Run haddock (hack: use the Setup script from package-tests!)
(cd Cabal && timed cabal act-as-setup --build-type=Simple -- haddock --builddir=${CABAL_BDIR})

# Redo the package tests with different versions of GHC
# TODO: reenable me
#   if [ "x$TEST_OLDER" = "xYES" -a "x$TRAVIS_OS_NAME" = "xlinux" ]; then
#       CABAL_PACKAGETESTS_WITH_GHC=/opt/ghc/7.0.4/bin/ghc \
#           ./dist/setup/setup test package-tests --show-details=streaming
#       CABAL_PACKAGETESTS_WITH_GHC=/opt/ghc/7.2.2/bin/ghc \
#           ./dist/setup/setup test package-tests --show-details=streaming
#   fi

# Check for package warnings
(cd Cabal && timed cabal check)

# Test that an sdist can be created
(cd Cabal && timed cabal sdist --builddir=${CABAL_BDIR})

unset CABAL_BUILDDIR

# ---------------------------------------------------------------------
# cabal-install
# ---------------------------------------------------------------------

# Setting the build directory here helps avoid sdist bugs.
export CABAL_BUILDDIR="${CABAL_INSTALL_BDIR}"

timed cabal new-build cabal-install:cabal \
                      cabal-install:integration-tests \
                      cabal-install:integration-tests2 \
                      cabal-install:unit-tests \
                      cabal-install:solver-quickcheck

# Run tests
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/unit-tests/unit-tests         $TEST_OPTIONS)
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/solver-quickcheck/solver-quickcheck  $TEST_OPTIONS --quickcheck-tests=1000)
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/integration-tests/integration-tests  $TEST_OPTIONS)
(cd cabal-install && timed ${CABAL_INSTALL_BDIR}/build/integration-tests2/integration-tests2 $TEST_OPTIONS)

# Haddock
(cd cabal-install && timed ${CABAL_INSTALL_SETUP} haddock --builddir=${CABAL_INSTALL_BDIR} )

(cd cabal-install && timed cabal check)
(cd cabal-install && timed cabal sdist -v3 --builddir=${CABAL_INSTALL_BDIR})

unset CABAL_BUILDDIR

# Check what we got
${CABAL_INSTALL_BDIR}/build/cabal/cabal --version
