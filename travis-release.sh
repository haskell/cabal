#!/bin/sh

# NB: the '|| exit $?' workaround is required on old broken versions of bash
# that ship with OS X. See https://github.com/haskell/cabal/pull/3624 and
# http://stackoverflow.com/questions/14970663/why-doesnt-bash-flag-e-exit-when-a-subshell-fails

. ./travis-common.sh

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
# Cabal
# ---------------------------------------------------------------------

timed cabal new-build $jobs cabal-install:cabal

# Check what we got
${CABAL_INSTALL_BDIR}/build/cabal/cabal --version

# Copy it somewhere easy to find
mkdir $HOME/cabal-install-SNAPSHOT
cp ${CABAL_INSTALL_BDIR}/build/cabal/cabal $HOME/cabal-install-SNAPSHOT
