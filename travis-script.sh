#!/bin/sh

# ATTENTION! Before editing this file, maybe you can make a
# separate script to do your test?  We don't want individual
# Travis builds to take too long (they time out at 50min and
# it's generally unpleasant if the build takes that long.)
# If you make a separate matrix entry in .travis.yml it can
# be run in parallel.

# NB: the '|| exit $?' workaround is required on old broken versions of bash
# that ship with OS X. See https://github.com/haskell/cabal/pull/3624 and
# http://stackoverflow.com/questions/14970663/why-doesnt-bash-flag-e-exit-when-a-subshell-fails

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

# ---------------------------------------------------------------------
# Parse options
# ---------------------------------------------------------------------

usage() {
    echo -e -n "Usage: `basename $0`\n-j  jobs\n"
}

jobs="-j1"
while getopts ":hj:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        j)
            jobs="-j$OPTARG"
            ;;
        :)
            # Argument-less -j
            if [ "$OPTARG" = "j" ]; then
                jobs="-j"
            fi
            ;;
        \?)
            echo "Invalid option: $OPTARG"
            usage
            exit 1
            ;;
    esac
done
shift $((OPTIND-1))

# Do not try to use -j with GHC older than 7.8
case $GHCVER in
    7.4*|7.6*)
        jobs=""
        ;;
    *)
        ;;
esac

# ---------------------------------------------------------------------
# Update the Cabal index
# ---------------------------------------------------------------------

timed cabal update

# ---------------------------------------------------------------------
# Install executables if necessary
# ---------------------------------------------------------------------

if ! command -v happy; then
    timed cabal install $jobs happy
fi

# ---------------------------------------------------------------------
# Setup our local project
# ---------------------------------------------------------------------

cp cabal.project.travis cabal.project.local

# hackage-repo-tool is a bit touchy to install on GHC 8.0, so instead we
# do it via new-build.  See also cabal.project.travis.  The downside of
# doing it this way is that the build product cannot be cached, but
# hackage-repo-tool is a relatively small package so it's good.
timed cabal unpack hackage-repo-tool-${HACKAGE_REPO_TOOL_VERSION}

# ---------------------------------------------------------------------
# Cabal
# ---------------------------------------------------------------------

# Needed to work around some bugs in nix-local-build code.
export CABAL_BUILDDIR="${CABAL_BDIR}"

if [ "x$CABAL_INSTALL_ONLY" != "xYES" ] ; then
    # We're doing a full build and test of Cabal

    # NB: Best to do everything for a single package together as it's
    # more efficient (since new-build will uselessly try to rebuild
    # Cabal otherwise).
    if [ "x$PARSEC" = "xYES" ]; then
      timed cabal new-build $jobs -fparsec Cabal Cabal:unit-tests Cabal:parser-tests Cabal:parser-hackage-tests
    else
      timed cabal new-build $jobs Cabal Cabal:unit-tests
    fi

    # Run haddock
    (cd Cabal && timed cabal act-as-setup --build-type=Simple -- haddock --builddir=${CABAL_BDIR}) || exit $?

    # Check for package warnings
    (cd Cabal && timed cabal check) || exit $?
fi

unset CABAL_BUILDDIR

# Build and run the package tests

export CABAL_BUILDDIR="${CABAL_TESTSUITE_BDIR}"

# NB: We always build this test runner, because it is used
# both by Cabal and cabal-install
timed cabal new-build $jobs cabal-testsuite:cabal-tests

unset CABAL_BUILDDIR

if [ "x$CABAL_LIB_ONLY" = "xYES" ]; then
    # If this fails, we WANT to fail, because the tests will not be running then
    (timed ./travis/upload.sh) || exit $?
    exit 0;
fi

# ---------------------------------------------------------------------
# cabal-install
# ---------------------------------------------------------------------

# Needed to work around some bugs in nix-local-build code.
export CABAL_BUILDDIR="${CABAL_INSTALL_BDIR}"

if [ "x$DEBUG_EXPENSIVE_ASSERTIONS" = "xYES" ]; then
    CABAL_INSTALL_FLAGS=-fdebug-expensive-assertions
fi

timed cabal new-build $jobs $CABAL_INSTALL_FLAGS \
                      cabal-install:cabal \
                      cabal-install:integration-tests \
                      cabal-install:integration-tests2 \
                      cabal-install:unit-tests \
                      cabal-install:solver-quickcheck \
                      cabal-install:memory-usage-tests

timed cabal new-build $jobs hackage-repo-tool

# Haddock
(cd cabal-install && timed ${CABAL_INSTALL_SETUP} haddock --builddir=${CABAL_INSTALL_BDIR} ) || exit $?

(cd cabal-install && timed cabal check) || exit $?

unset CABAL_BUILDDIR

# Check what we got
${CABAL_INSTALL_BDIR}/build/cabal/cabal --version

# If this fails, we WANT to fail, because the tests will not be running then
(timed ./travis/upload.sh) || exit $?
