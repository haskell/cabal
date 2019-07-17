#!/bin/sh
# shellcheck disable=SC2086

# default config
#######################################################################

HC=ghc-8.2.2
CABAL=cabal
CABALPLAN=cabal-plan
JOBS=4
CABALTESTS=true
CABALINSTALLTESTS=true
CABALSUITETESTS=true
CABALONLY=false
DEPSONLY=false
VERBOSE=false

# Help
#######################################################################

show_usage() {
cat <<EOF
./validate.sh - build & test

Usage: ./validate.sh [ -j JOBS | -l | -C | -c | -s | -w HC | -x CABAL | -y CABALPLAN | -d | -v ]
  A script which runs all the tests.

Available options:
  -j JOBS        cabal v2-build -j argument (default: $JOBS)
  -l             Test Cabal-the-library only (default: $CABALONLY)
  -C             Don't run Cabal tests (default: $CABALTESTS)
  -c             Don't run cabal-install tests (default: $CABALINSTALLTESTS)
  -s             Don't run cabal-testsuite tests (default: $CABALSUITETESTS)
  -w HC          With compiler
  -x CABAL       With cabal-install
  -y CABALPLAN   With cabal-plan
  -d             Build dependencies only
  -v             Verbose
EOF
exit 0
}

# "library"
#######################################################################

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

    if $VERBOSE; then
        "$@" 2>&1
    else
        "$@" > "$OUTPUT" 2>&1
    fi
    # echo "MOCK" > "$OUTPUT"
    RET=$?

    end_time=$(date +%s)
    duration=$((end_time - start_time))
    tduration=$((end_time - JOB_START_TIME))

    if [ $RET -eq 0 ]; then
        if ! $VERBOSE; then
            # if output is relatively short, show everything
            if [ "$(wc -l < "$OUTPUT")" -le 50 ]; then
                cat "$OUTPUT"
            else
                echo "..."
                tail -n 20 "$OUTPUT"
            fi

            rm -f "$OUTPUT"
        fi

        echo "$GREEN<<< $PRETTYCMD $RESET ($duration/$tduration sec)"

        # bottom-margin
        echo ""
    else
        if ! $VERBOSE; then
            cat "$OUTPUT"
        fi

        echo "$RED<<< $PRETTYCMD $RESET ($duration/$tduration sec, $RET)"
        echo "$RED<<< $* $RESET ($duration/$tduration sec, $RET)"
        rm -f "$OUTPUT"
        exit 1
    fi
}

footer() {
    JOB_END_TIME=$(date +%s)
    tduration=$((JOB_END_TIME - JOB_START_TIME))

    echo "$CYAN=== END ============================================ $(date +%T) === $RESET"
    echo "$CYAN!!! Validation took $tduration seconds. $RESET"
}

# getopt
#######################################################################

while getopts 'j:lCcsw:x:y:dv' flag; do
    case $flag in
        j) JOBS="$OPTARG"
            ;;
        l) CABALONLY=true
            ;;
        C) CABALTESTS=false
            ;;
        c) CABALINSTALLTESTS=false
            ;;
        s) CABALSUITETESTS=false
            ;;
        w) HC="$OPTARG"
            ;;
        x) CABAL="$OPTARG"
            ;;
        y) CABALPLAN="$OPTARG"
            ;;
        d) DEPSONLY=true
            ;;
        v) VERBOSE=true
            ;;
        ?) show_usage
            ;;
    esac
done

shift $((OPTIND - 1))

# header
#######################################################################

if [ "xhelp" = "x$1" ]; then
    show_usage;
fi

TESTSUITEJOBS="-j$JOBS"
JOBS="-j$JOBS"

# assume compiler is GHC
RUNHASKELL=$(echo $HC | sed -E 's/ghc(-[0-9.]*)$/runghc\1/')

echo "$CYAN=== validate.sh ======================================== $(date +%T) === $RESET"

cat <<EOF
compiler:            $HC
runhaskell           $RUNHASKELL
cabal-install:       $CABAL
cabal-plan:          $CABALPLAN
jobs:                $JOBS
Cabal tests:         $CABALTESTS
cabal-install tests: $CABALINSTALLTESTS
cabal-testsuite:     $CABALSUITETESTS
library only:        $CABALONLY
dependencies only:   $DEPSONLY
verbose:             $VERBOSE

EOF

timed $HC --version
timed $CABAL --version
timed $CABALPLAN --version

# Basic setup
#######################################################################

# NOTE: This should match cabal-testsuite version
CABAL_VERSION="3.1.0.0"

if [ "$(uname)" = "Linux" ]; then
    ARCH="x86_64-linux"
else
    ARCH="x86_64-osx"
fi

if $CABALONLY; then
    PROJECTFILE=cabal.project.validate.libonly
else
    PROJECTFILE=cabal.project.validate
fi

BASEHC=$(basename $HC)
BUILDDIR=dist-newstyle-validate-$BASEHC
CABAL_TESTSUITE_BDIR="$(pwd)/$BUILDDIR/build/$ARCH/$BASEHC/cabal-testsuite-${CABAL_VERSION}"

CABALNEWBUILD="${CABAL} v2-build $JOBS -w $HC --builddir=$BUILDDIR --project-file=$PROJECTFILE"
CABALPLAN="${CABALPLAN} --builddir=$BUILDDIR"

# SCRIPT
#######################################################################

if ! $CABALONLY; then

echo "$CYAN=== make cabal-install-dev ============================= $(date +%T) === $RESET"

# make cabal-install-dev
timed ${RUNHASKELL} cabal-dev-scripts/src/Preprocessor.hs -o cabal-install/cabal-install.cabal -f CABAL_FLAG_LIB cabal-install/cabal-install.cabal.pp

fi # CABALONLY

# Dependencies

if $DEPSONLY; then

echo "$CYAN=== dependencies  ====================================== $(date +%T) === $RESET"

timed $CABALNEWBUILD Cabal:lib:Cabal --enable-tests --disable-benchmarks --dep --dry-run || exit 1
timed $CABALNEWBUILD Cabal:lib:Cabal --enable-tests --disable-benchmarks --dep || exit 1
if $CABALTESTS; then
    timed $CABALNEWBUILD Cabal --enable-tests --disable-benchmarks --dep --dry-run || exit 1
    timed $CABALNEWBUILD Cabal --enable-tests --disable-benchmarks --dep || exit 1
fi

# Unfortunately we can not install cabal-install or cabal-testsuite dependencies:
# that would build Cabal-lib!

footer
exit

fi # DEPSONLY

# Cabal lib
#######################################################################

echo "$CYAN=== Cabal: build ======================================= $(date +%T) === $RESET"

timed $CABALNEWBUILD Cabal:lib:Cabal --enable-tests --disable-benchmarks --dry-run || exit 1
timed $CABALNEWBUILD Cabal:lib:Cabal --enable-tests --disable-benchmarks --dep || exit 1
timed $CABALNEWBUILD Cabal:lib:Cabal --enable-tests --disable-benchmarks || exit 1

if $CABALTESTS; then
echo "$CYAN=== Cabal: test ======================================== $(date +%T) === $RESET"

timed $CABALNEWBUILD Cabal:tests --enable-tests --disable-benchmarks --dry-run || exit 1
timed $CABALNEWBUILD Cabal:tests --enable-tests --disable-benchmarks --dep || exit 1
timed $CABALNEWBUILD Cabal:tests --enable-tests --disable-benchmarks || exit 1

CMD="$($CABALPLAN list-bin Cabal:test:unit-tests) $TESTSUITEJOBS --hide-successes --with-ghc=$HC"
(cd Cabal && timed $CMD) || exit 1

CMD="$($CABALPLAN list-bin Cabal:test:check-tests) $TESTSUITEJOBS --hide-successes"
(cd Cabal && timed $CMD) || exit 1

CMD="$($CABALPLAN list-bin Cabal:test:parser-tests) $TESTSUITEJOBS --hide-successes"
(cd Cabal && timed $CMD) || exit 1

CMD=$($CABALPLAN list-bin Cabal:test:hackage-tests)
(cd Cabal && timed $CMD read-fields) || exit 1
(cd Cabal && timed $CMD parsec d)    || exit 1
(cd Cabal && timed $CMD roundtrip k) || exit 1

fi # $CABALTESTS

if $CABALSUITETESTS; then

echo "$CYAN=== cabal-testsuite: build ============================= $(date +%T) === $RESET"

timed $CABALNEWBUILD cabal-testsuite --enable-tests --disable-benchmarks --dry-run || exit 1
timed $CABALNEWBUILD cabal-testsuite --enable-tests --disable-benchmarks --dep || exit 1
timed $CABALNEWBUILD cabal-testsuite --enable-tests --disable-benchmarks || exit 1

echo "$CYAN=== cabal-testsuite: Cabal test ======================== $(date +%T) === $RESET"

CMD="$($CABALPLAN list-bin cabal-testsuite:exe:cabal-tests) --builddir=$CABAL_TESTSUITE_BDIR $TESTSUITEJOBS --with-ghc=$HC --hide-successes"
(cd cabal-testsuite && timed $CMD) || exit 1

fi # CABALSUITETESTS (Cabal)

# If testing only library, stop here
if $CABALONLY; then
    footer
    exit
fi

# cabal-install
#######################################################################

echo "$CYAN=== cabal-install: build =============================== $(date +%T) === $RESET"

timed $CABALNEWBUILD cabal-install --enable-tests --disable-benchmarks --dry-run || exit 1

# For some reason this sometimes fails. So we try twice.
CMD="$CABALNEWBUILD cabal-install --enable-tests --disable-benchmarks"
(timed $CMD) || (timed $CMD) || exit 1


if $CABALINSTALLTESTS; then
echo "$CYAN=== cabal-install: test ================================ $(date +%T) === $RESET"

# this are sorted in asc time used, quicker tests first.
CMD="$($CABALPLAN list-bin cabal-install:test:solver-quickcheck) $TESTSUITEJOBS --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

# This doesn't work in parallel either
CMD="$($CABALPLAN list-bin cabal-install:test:unit-tests) -j1 --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

# Only single job, otherwise we fail with "Heap exhausted"
CMD="$($CABALPLAN list-bin cabal-install:test:memory-usage-tests) -j1 --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

# This test-suite doesn't like concurrency
CMD="$($CABALPLAN list-bin cabal-install:test:integration-tests2) -j1 --hide-successes --with-ghc=$HC"
(cd cabal-install && timed $CMD) || exit 1

fi # CABALINSTALLTESTS


if $CABALSUITETESTS; then
echo "$CYAN=== cabal-testsuite: cabal-install test ================ $(date +%T) === $RESET"

CMD="$($CABALPLAN list-bin cabal-testsuite:exe:cabal-tests) --builddir=$CABAL_TESTSUITE_BDIR --with-cabal=$($CABALPLAN list-bin cabal-install:exe:cabal) $TESTSUITEJOBS --hide-successes"
(cd cabal-testsuite && timed $CMD) || exit 1

fi # CABALSUITETESTS

# END
#######################################################################

footer

#######################################################################
