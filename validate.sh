#!/bin/sh
# shellcheck disable=SC2086

# This is a helper script to build and run tests locally
# It mimics appveyor.yml
#
# Simple usage:
#
#     $ HC=ghc-7.10.3 sh validate.sh
#
# Multiple ghcs (serial), this takes very long.
#
#     $ sh validate.sh ghc-7.6.3 ghc-7.8.4 ghc-7.10.3 ghc-8.0.2 ghc-8.2.2
#

# Loop thru compilers if given as an argument
if [ $# -ne 0 ]; then
    set -e
    for HC in "$@"; do
        export HC
        sh $0
    done
    exit 0
fi

HC=${HC-ghc-8.2.2}
JOBS=${JOBS--j4}
TESTSUITEJOBS=${TESTSUITEJOBS--j3}

CABAL_VERSION="2.1.0.0"
if [ "$(uname)" = "Linux" ]; then
    ARCH="x86_64-linux"
else
    ARCH="x86_64-osx"
fi

BUILDDIR=dist-newstyle-validate-$HC
CABAL_TESTSUITE_BDIR="$(pwd)/$BUILDDIR/build/$ARCH/$HC/cabal-testsuite-${CABAL_VERSION}"

CABALNEWBUILD="cabal new-build $JOBS -w $HC --builddir=$BUILDDIR --project-file=cabal.project.validate"
CABALPLAN="cabal-plan --builddir=$BUILDDIR"

OUTPUT=$(mktemp)

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;96m'
RESET='\033[0m' # No Color

JOB_START_TIME=$(date +%s)

timed() {
    PRETTYCMD=$(echo "$@" | sed -E 's/\/home[^ ]*\/([^\/])/**\/\1/g')
    echo "$BLUE>>> $PRETTYCMD $RESET"
    start_time=$(date +%s)

    "$@" > "$OUTPUT" 2>&1
    # echo "MOCK" > "$OUTPUT"
    RET=$?

    end_time=$(date +%s)
    duration=$((end_time - start_time))
    tduration=$((end_time - JOB_START_TIME))

    if [ $RET -eq 0 ]; then
        echo "$GREEN<<< $PRETTYCMD $RESET ($duration/$tduration sec)"
        tail -n 5 "$OUTPUT"
        rm -f "$OUTPUT"

        # bottom-margin
        echo ""
    else
        echo "$RED<<< $PRETTYCMD $RESET ($duration/$tduration sec, $RET)"
        cat "$OUTPUT"
        echo "$RED<<< $PRETTYCMD $RESET ($duration/$tduration sec, $RET)"
        rm -f "$OUTPUT"
        exit 1
    fi
}

# Info
echo "$CYAN!!! Validating with $HC $RESET"

timed ghc --version
timed cabal --version
timed cabal-plan --version


# Cabal
echo "$CYAN=== Cabal: build ======================================= $(date +%T) === $RESET"

timed $CABALNEWBUILD Cabal:lib:Cabal --enable-tests --disable-benchmarks --dry-run || exit 1
timed $CABALNEWBUILD Cabal:lib:Cabal --enable-tests --disable-benchmarks --dep || exit 1
timed $CABALNEWBUILD Cabal:lib:Cabal --enable-tests --disable-benchmarks || exit 1

# Environment files interfere with legacy Custom setup builds in sandbox
# https://github.com/haskell/cabal/issues/4642
rm -rf .ghc.environment.*

## Cabal tests
echo "$CYAN=== Cabal: test ======================================== $(date +%T) === $RESET"

timed $CABALNEWBUILD Cabal:tests --enable-tests --disable-benchmarks --dry-run || exit 1
timed $CABALNEWBUILD Cabal:tests --enable-tests --disable-benchmarks || exit 1
rm -rf .ghc.environment.*

CMD="$($CABALPLAN list-bin Cabal:test:unit-tests) $TESTSUITEJOBS --hide-successes"
(cd Cabal && timed $CMD) || exit 1

CMD="$($CABALPLAN list-bin Cabal:test:check-tests) $TESTSUITEJOBS --hide-successes"
(cd Cabal && timed $CMD) || exit 1

CMD="$($CABALPLAN list-bin Cabal:test:parser-tests) $TESTSUITEJOBS --hide-successes"
(cd Cabal && timed $CMD) || exit 1

CMD=$($CABALPLAN list-bin Cabal:test:parser-hackage-tests)
(cd Cabal && timed $CMD parse-parsec d) || exit 1
(cd Cabal && timed $CMD roundtrip k) || exit 1


# cabal-testsuite
# cabal test ssuite is run first
echo "$CYAN=== cabal-install cabal-testsuite: build =============== $(date +%T) === $RESET"

timed $CABALNEWBUILD all --enable-tests --disable-benchmarks --dry-run || exit 1

# For some reason this sometimes fails. So we try twice.
CMD="timed $CABALNEWBUILD all --enable-tests --disable-benchmarks"
($CMD || $CMD || exit 1)
rm -rf .ghc.environment.*


# cabal-install tests
echo "$CYAN=== cabal-install: test ================================ $(date +%T) === $RESET"

# this are sorted in asc time used, quicker tests first.
CMD="$($CABALPLAN list-bin cabal-install:test:solver-quickcheck) $TESTSUITEJOBS --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

CMD="$($CABALPLAN list-bin cabal-install:test:unit-tests) $TESTSUITEJOBS --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

# Only single job, otherwise we fail with "Heap exhausted"
CMD="$($CABALPLAN list-bin cabal-install:test:memory-usage-tests) -j1 --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

# This test-suite doesn't like concurrency
CMD="$($CABALPLAN list-bin cabal-install:test:integration-tests2) -j1 --hide-successes"
(cd cabal-install && timed $CMD) || exit 1


# cabal-testsuite tests
echo "$CYAN=== cabal-testsuite: test ============================== $(date +%T) === $RESET"

CMD="$($CABALPLAN list-bin cabal-testsuite:exe:cabal-tests) --builddir=$CABAL_TESTSUITE_BDIR --with-cabal=$($CABALPLAN list-bin cabal-install:exe:cabal) $TESTSUITEJOBS --hide-successes"
(cd cabal-testsuite && timed $CMD) || exit 1


# Footer
JOB_END_TIME=$(date +%s)
tduration=$((JOB_END_TIME - JOB_START_TIME))

echo "$CYAN!!! Validation took $tduration seconds. $RESET"
