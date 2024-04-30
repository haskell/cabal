#!/bin/sh
# shellcheck disable=SC2086

# default config
#######################################################################

# We use the default ghc in PATH as default
# Use the ghc-x.y.z trigger several errors in windows:
# * It triggers the max path length issue:
#   See https://github.com/haskell/cabal/issues/6271#issuecomment-1065102255
# * It triggers a `createProcess: does not exist` error in units tests
#   See https://github.com/haskell/cabal/issues/8049
HC=ghc
CABAL=cabal
JOBS=4
LIBTESTS=true
CLITESTS=true
CABALSUITETESTS=true
LIBONLY=false
DEPSONLY=false
DOCTEST=false
BENCHMARKS=false
VERBOSE=false
HACKAGETESTSALL=false

TARGETS=""
STEPS=""
EXTRAHCS=""

LISTSTEPS=false

# Help
#######################################################################

show_usage() {
cat <<EOF
./validate.sh - build & test

Usage: ./validate.sh [options]
  A script which runs all the tests.

Available options:
  -j, --jobs JOBS                   cabal build -j argument (default:  $JOBS)
      --libonly                     Test only Cabal-the-library
      --cli                         Test both Cabal-the-library and cabal-install
      --(no-)run-lib-tests          Run library tests
      --(no-)run-cli-tests          Run client tests
      --(no-)run-lib-suite          Run cabal-testsuite with library
      --(no-)run-cli-suite          Run cabal-testsuite with client
  -w, --with-compiler HC            With compiler
      --with-cabal CABAL            With cabal-install
      --extra-hc HC                 Extra compiler to run test-suite with
      --(no-)doctest                Run doctest on library
      --(no-)solver-benchmarks      Build and trial run solver-benchmarks
      --complete-hackage-tests      Run hackage-tests on complete Hackage data
      --partial-hackage-tests       Run hackage-tests on parts of Hackage data
  -v, --verbose                     Verbose output
  -q, --quiet                       Less output
  -s, --step STEP                   Run only specific step (can be specified multiple times)
      --list-steps                  List steps and build-targets and exit
      --help                        Print this message and exit
EOF
}

# "library"
#######################################################################

OUTPUT=$(mktemp)

# `red` and `green` are used to output also the spent time in white at the end.
# `blue` and `cyan` are used to print the spawned command, so they only have one
# argument.
red () {
  printf "\033[0;31m%s\033[0m %s \n" "$1" "$2"
}
green () {
  printf "\033[0;32m%s\033[0m %s \n" "$1" "$2"
}
blue () {
  printf "\033[0;34m%s\033[0m\n" "$1"
}
cyan () {
  printf "\033[0;96m%s\033[0m\n" "$1"
}

JOB_START_TIME=$(date +%s)

timed() {
    PRETTYCMD=$(echo "$@" | sed -E 's/\/home[^ ]*\/([^\/])/**\/\1/g')
    blue "$PRETTYCMD"
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

        green "<<< $PRETTYCMD" "($duration/$tduration sec)"

        # bottom-margin
        echo ""
    else
        if ! $VERBOSE; then
            cat "$OUTPUT"
        fi

        red "<<< $PRETTYCMD" "($duration/$tduration sec, $RET)"
        red "<<< $*" "($duration/$tduration sec, $RET)"
        rm -f "$OUTPUT"
        exit 1
    fi
}

print_header() {
    TITLE=$1
    TITLEPAT="$(echo "$TITLE"|sed 's:.:=:g')"
    cyan "===X========================================================================== $(date +%T) ===" \
      | sed "s#X$TITLEPAT=# $TITLE #"

}

# getopt
#######################################################################

while [ $# -gt 0 ]; do
    arg=$1
    case $arg in
        --help)
            show_usage
            exit
            ;;
        -j|--jobs)
            JOBS="$2"
            shift
            shift
            ;;
        --lib-only)
            LIBONLY=true
            shift
            ;;
        --cli)
            LIBONLY=false
            shift
            ;;
        --run-lib-tests)
            LIBTESTS=true
            shift
            ;;
        --no-run-lib-tests)
            LIBTESTS=false
            shift
            ;;
        --run-cli-tests)
            CLITESTS=true
            shift
            ;;
        --no-run-cli-tests)
            CLITESTS=false
            shift
            ;;
        --run-lib-suite)
            LIBSUITE=true
            shift
            ;;
        --no-run-lib-suite)
            LIBSUITE=false
            shift
            ;;
        --run-cli-suite)
            CLISUITE=true
            shift
            ;;
        --no-run-cli-suite)
            CLISUITE=false
            shift
            ;;
        -w|--with-compiler)
            HC=$2
            shift
            shift
            ;;
        --with-cabal)
            CABAL=$2
            shift
            shift
            ;;
        --extra-hc)
            EXTRAHCS="$EXTRAHCS $2"
            shift
            shift
            ;;
        --doctest)
            DOCTEST=true
            shift
            ;;
        --no-doctest)
            DOCTEST=false
            shift
            ;;
        --solver-benchmarks)
            BENCHMARKS=true
            shift
            ;;
        --no-solver-benchmarks)
            BENCHMARKS=false
            shift
            ;;
        --complete-hackage-tests)
            HACKAGETESTSALL=true
            shift
            ;;
        --partial-hackage-tests)
            HACKAGETESTSALL=false
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -q|--quiet)
            VERBOSE=false
            shift
            ;;
        -s|--step)
            STEPS="$STEPS $2"
            shift
            shift
            ;;
        --list-steps)
            LISTSTEPS=true
            shift
            ;;
        *)
            echo "Unknown option $arg"
            exit 1
    esac
done

# calculate steps and build targets
#######################################################################

# If there are no explicit steps given calculate them
if $LIBONLY; then
    CLITESTS=false
    CLISUITE=false
    BENCHMARKS=false
fi

if [ -z "$STEPS" ]; then
    STEPS="print-config print-tool-versions"
    STEPS="$STEPS build"
    if $DOCTEST;    then STEPS="$STEPS doctest";   fi
    if $LIBTESTS;   then STEPS="$STEPS lib-tests"; fi
    if $LIBSUITE;   then STEPS="$STEPS lib-suite"; fi
    if $LIBSUITE && [ -n "$EXTRAHCS" ];
                    then STEPS="$STEPS lib-suite-extras"; fi
    if $CLITESTS;   then STEPS="$STEPS cli-tests"; fi
    if $CLISUITE;   then STEPS="$STEPS cli-suite"; fi
    if $BENCHMARKS; then STEPS="$STEPS solver-benchmarks-tests solver-benchmarks-run"; fi
    STEPS="$STEPS time-summary"
fi

TARGETS="Cabal Cabal-hooks cabal-testsuite Cabal-tests Cabal-QuickCheck Cabal-tree-diff Cabal-described"
if ! $LIBONLY;  then TARGETS="$TARGETS cabal-install cabal-install-solver cabal-benchmarks"; fi
if $BENCHMARKS; then TARGETS="$TARGETS solver-benchmarks"; fi

if $LISTSTEPS; then
  echo "Targets: $TARGETS"
  echo "Steps:   $STEPS"
  exit
fi

# Adjust runtime configuration
#######################################################################

TESTSUITEJOBS="-j$JOBS"
JOBS="-j$JOBS"

# assume compiler is GHC
RUNHASKELL=$(echo "$HC" | sed -E 's/ghc(-[0-9.]*)$/runghc\1/')

case "$(uname)" in
    MINGW64*)
        ARCH="x86_64-windows"
        ;;
    Linux   )
        ARCH="x86_64-linux"
        ;;
    *)
        ARCH="x86_64-osx"
        ;;
esac

if $LIBONLY; then
    PROJECTFILE=cabal.validate-libonly.project
else
    PROJECTFILE=cabal.validate.project
fi

BASEHC=ghc-$($HC --numeric-version)
BUILDDIR=dist-newstyle-validate-$BASEHC
CABAL_TESTSUITE_BDIR="$(pwd)/$BUILDDIR/build/$ARCH/$BASEHC/cabal-testsuite-3"

CABALNEWBUILD="${CABAL} build $JOBS -w $HC --builddir=$BUILDDIR --project-file=$PROJECTFILE"
CABALLISTBIN="${CABAL} list-bin --builddir=$BUILDDIR --project-file=$PROJECTFILE"

# header
#######################################################################

step_print_config() {
print_header print-config

cat <<EOF
compiler:            $HC
runhaskell:          $RUNHASKELL
cabal-install:       $CABAL
jobs:                $JOBS
Cabal tests:         $LIBTESTS
cabal-install tests: $CLITESTS
cabal-testsuite:     $CABALSUITETESTS
library only:        $LIBONLY
dependencies only:   $DEPSONLY
doctest:             $DOCTEST
benchmarks:          $BENCHMARKS
verbose:             $VERBOSE
extra compilers:     $EXTRAHCS

EOF
}

step_print_tool_versions() {
print_header print-tool-versions

timed "$HC" --version
timed "$CABAL" --version

for EXTRAHC in $EXTRAHCS; do
    timed "$EXTRAHC" --version
done
}

step_time_summary() {
    print_header END

    JOB_END_TIME=$(date +%s)
    tduration=$((JOB_END_TIME - JOB_START_TIME))

    cyan "!!! Validation took $tduration seconds."
}

# build
#######################################################################

step_build() {
print_header "build"
print_header "Step Build: dry run"
timed $CABALNEWBUILD $TARGETS --dry-run || exit 1
print_header "Step Build: full build plan (cached and to-be-built dependencies):"
jq -r '."install-plan" | map(."pkg-name" + "-" + ."pkg-version" + " " + ."component-name") | join("\n")' "$BUILDDIR/cache/plan.json"
print_header "Step Build: actual build"
timed $CABALNEWBUILD $TARGETS || exit 1
}

# Cabal lib
#######################################################################

step_doctest() {
print_header "Cabal: doctest"
cabal-env --name doctest-Cabal --transitive QuickCheck
cabal-env --name doctest-Cabal array bytestring containers deepseq directory filepath pretty process time binary unix text parsec mtl
timed doctest -package-env=doctest-Cabal --fast Cabal/Distribution Cabal/Language
}

step_lib_tests() {
print_header "Cabal: tests"

CMD="$($CABALLISTBIN Cabal-tests:test:unit-tests) $TESTSUITEJOBS --hide-successes --with-ghc=$HC"
(cd Cabal-tests && timed $CMD) || exit 1

CMD="$($CABALLISTBIN Cabal-tests:test:check-tests) $TESTSUITEJOBS --hide-successes"
(cd Cabal-tests && timed $CMD) || exit 1

CMD="$($CABALLISTBIN Cabal-tests:test:parser-tests) $TESTSUITEJOBS --hide-successes"
(cd Cabal-tests && timed $CMD) || exit 1

CMD="$($CABALLISTBIN Cabal-tests:test:rpmvercmp) $TESTSUITEJOBS --hide-successes"
(cd Cabal-tests && timed $CMD) || exit 1

CMD="$($CABALLISTBIN Cabal-tests:test:no-thunks-test) $TESTSUITEJOBS --hide-successes"
(cd Cabal-tests && timed $CMD) || exit 1

CMD=$($CABALLISTBIN Cabal-tests:test:hackage-tests)
(cd Cabal-tests && timed $CMD read-fields) || exit 1
if $HACKAGETESTSALL; then
    (cd Cabal-tests && timed $CMD parsec)    || exit 1
    (cd Cabal-tests && timed $CMD roundtrip) || exit 1
else
    (cd Cabal-tests && timed $CMD parsec d)    || exit 1
    (cd Cabal-tests && timed $CMD roundtrip k) || exit 1
fi
}

# Cabal cabal-testsuite
#######################################################################

step_lib_suite() {
print_header "Cabal: cabal-testsuite"

CMD="$($CABALLISTBIN cabal-testsuite:exe:cabal-tests) --builddir=$CABAL_TESTSUITE_BDIR $TESTSUITEJOBS --with-ghc=$HC --hide-successes"
(cd cabal-testsuite && timed $CMD) || exit 1
}

step_lib_suite_extras() {
for EXTRAHC in $EXTRAHCS; do

CMD="$($CABALLISTBIN cabal-testsuite:exe:cabal-tests) --builddir=$CABAL_TESTSUITE_BDIR $TESTSUITEJOBS --with-ghc=$EXTRAHC --hide-successes"
(cd cabal-testsuite && timed $CMD) || exit 1

done
}

# cabal-install
#######################################################################

step_cli_tests() {
print_header "cabal-install: tests"

# this are sorted in asc time used, quicker tests first.
CMD="$($CABALLISTBIN cabal-install:test:long-tests) $TESTSUITEJOBS --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

# This doesn't work in parallel either
CMD="$($CABALLISTBIN cabal-install:test:unit-tests) -j1 --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

# Only single job, otherwise we fail with "Heap exhausted"
CMD="$($CABALLISTBIN cabal-install:test:mem-use-tests) -j1 --hide-successes"
(cd cabal-install && timed $CMD) || exit 1

# This test-suite doesn't like concurrency
CMD="$($CABALLISTBIN cabal-install:test:integration-tests2) -j1 --hide-successes --with-ghc=$HC"
(cd cabal-install && timed $CMD) || exit 1
}

# cabal-install cabal-testsuite
#######################################################################

step_cli_suite() {
print_header "cabal-install: cabal-testsuite"

CMD="$($CABALLISTBIN cabal-testsuite:exe:cabal-tests) --builddir=$CABAL_TESTSUITE_BDIR --with-cabal=$($CABALLISTBIN cabal-install:exe:cabal) $TESTSUITEJOBS  --with-ghc=$HC --hide-successes --intree-cabal-lib=$PWD --test-tmp=$PWD/testdb"
(cd cabal-testsuite && timed $CMD) || exit 1
}

# solver-benchmarks
#######################################################################

step_solver_benchmarks_tests() {
print_header "solver-benchmarks: test"

CMD="$($CABALLISTBIN solver-benchmarks:test:unit-tests)"
(cd Cabal && timed $CMD) || exit 1
}

step_solver_benchmarks_run() {
print_header "solver-benchmarks: run"

SOLVEPKG=Chart-diagrams
CMD="$($CABALLISTBIN solver-benchmarks:exe:hackage-benchmark) --cabal1=$CABAL --cabal2=$($CABALLISTBIN cabal-install:exe:cabal) --trials=5 --packages=$SOLVEPKG --print-trials"
(cd Cabal && timed $CMD) || exit 1
}

# Steps dispatcher
#######################################################################

for step in $STEPS; do
    case $step in
        print-config)             step_print_config            ;;
        print-tool-versions)      step_print_tool_versions     ;;
        build)                    step_build                   ;;
        doctest)                  step_doctest                 ;;
        lib-tests)                step_lib_tests               ;;
        cli-tests)                step_cli_tests               ;;
        lib-suite)                step_lib_suite               ;;
        lib-suite-extras)         step_lib_suite_extras        ;;
        cli-suite)                step_cli_suite               ;;
        solver-benchmarks-tests)  step_solver_benchmarks_tests ;;
        solver-benchmarks-run)    step_solver_benchmarks_run   ;;
        time-summary)             step_time_summary            ;;
        *)
            echo "Invalid step $step"
            exit 1
            ;;
    esac
done

#######################################################################
