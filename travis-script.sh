#!/bin/sh
set -ev

usage() {
    echo "Usage: travis-script.sh"
    echo "-h  Print this help string"
    echo "-j  Number of concurrent workers to use (Default: 1)"
    echo "    -j without an argument will use all available cores"
}

jobs="-j1"
# We use a separate jobs var for bootstrap.sh because the arg parser in
# bootstrap.sh wouldn't be able to interpret -j1 (or any -jN).
bootstrap_jobs="-j 1"
while getopts ":hj:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        j)
            jobs="-j$OPTARG"
            bootstrap_jobs="-j $OPTARG"
            ;;
        :)
            # Argument-less -j
            if [ "$OPTARG" = "j" ]; then
                jobs="-j"
                bootstrap_jobs="-j"
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
        bootstrap_jobs=""
        ;;
    *)
        ;;
esac

# ---------------------------------------------------------------------
# Timing
# ---------------------------------------------------------------------

timed() {
    echo "$1"
    start_time=`date +%s`
    $2
    end_time=`date +%s`
    echo "$1" took $(expr $end_time - $start_time) seconds.
    echo "----"
}

# ---------------------------------------------------------------------
# Bootstrap cabal, to verify bootstrap.sh script works.
# ---------------------------------------------------------------------

bootstrap() {
    OLD_CWD=$PWD

    # Bootstrap
    cd cabal-install
    env EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh $bootstrap_jobs --no-doc
    ~/.cabal/bin/cabal --version

    # Move cabal for local use.
    mkdir ~/fresh-cabal
    mv -i ~/.cabal/bin/cabal ~/fresh-cabal/

    # Clean up after ourselves.
    rm -r ~/.ghc ~/.cabal

    # From here on we use the freshly built cabal executable.
    export PATH="$HOME/fresh-cabal/:$PATH"

    cd "$OLD_CWD"
}

install_parsec() {
    # Initial working directory: base directory of Git repository

    cabal update

    # We depend on parsec nowadays, which isn't distributed with GHC <8.0
    if [ "$PARSEC_BUNDLED" != "YES" ]; then
        cabal install $jobs parsec
    fi
}

timed "bootstrap.sh script" bootstrap
timed "Installing parsec" install_parsec

# ---------------------------------------------------------------------
# Check that auto-generated files/fields are up to date.
# ---------------------------------------------------------------------

generated_files() {
    # Regenerate the CONTRIBUTORS file.
    # Currently doesn't work because Travis uses --depth=50 when cloning.
    #./Cabal/misc/gen-authors.sh > AUTHORS

    # Regenerate the 'extra-source-files' field in Cabal.cabal.
    cd Cabal
    ./misc/gen-extra-source-files.sh Cabal.cabal

    # Regenerate the 'extra-source-files' field in cabal-install.cabal.
    cd ../cabal-install
    ../Cabal/misc/gen-extra-source-files.sh cabal-install.cabal
    cd ..

    # Fail if the diff is not empty.
    ./Cabal/misc/travis-diff-files.sh
}

timed "Generated files check" generated_files

# ---------------------------------------------------------------------
# Cabal
# ---------------------------------------------------------------------

# The following scriptlet checks that the resulting source distribution can be
# built & installed.
install_from_tarball() {
   SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}') ;
   export SRC_TGZ
   if [ -f "dist/$SRC_TGZ" ]; then
      cabal install $jobs "dist/$SRC_TGZ" -v2;
   else
      echo "expected 'dist/$SRC_TGZ' not found";
      exit 1;
   fi
}

cabal_dependencies() {
    cd Cabal

    # Build the setup script in the same way that cabal-install would:
    mkdir -p ./dist/setup
    cp Setup.hs ./dist/setup/setup.hs
    ghc --make \
        $jobs \
        -odir ./dist/setup -hidir ./dist/setup -i -i. \
        ./dist/setup/setup.hs -o ./dist/setup/setup \
        -Wall -Werror -threaded

    # Install test dependencies only after setup is built
    cabal install $jobs --only-dependencies --enable-tests --enable-benchmarks
}

cabal_build() {
    ./dist/setup/setup configure \
        --user --ghc-option=-Werror --enable-tests --enable-benchmarks \
        -v2 # -v2 provides useful information for debugging

    # Build all libraries and executables (including tests/benchmarks)
    ./dist/setup/setup build $jobs
    ./dist/setup/setup haddock # see https://github.com/haskell/cabal/issues/2198
    ./dist/setup/setup test --show-details=streaming --test-option=--hide-successes

    # Redo the package tests with different versions of GHC
    if [ "x$TEST_OLDER" = "xYES" ]; then
        CABAL_PACKAGETESTS_WITH_GHC=/opt/ghc/7.0.4/bin/ghc \
            ./dist/setup/setup test package-tests --show-details=streaming
        CABAL_PACKAGETESTS_WITH_GHC=/opt/ghc/7.2.2/bin/ghc \
            ./dist/setup/setup test package-tests --show-details=streaming
    fi

    cabal check
    cabal sdist   # tests that a source-distribution can be generated
    install_from_tarball

    cd ..
}

timed "Cabal dependencies" cabal_dependencies
timed "Cabal build" cabal_build

# ---------------------------------------------------------------------
# cabal-install
# ---------------------------------------------------------------------

cabalinstall_dependencies() {
    cd cabal-install

    cabal install $jobs happy
    cabal install $jobs --only-dependencies --enable-tests --enable-benchmarks
}

cabalinstall_build() {
    cabal configure \
        --user --ghc-option=-Werror --enable-tests --enable-benchmarks \
        -v2 # -v2 provides useful information for debugging
    cabal build $jobs
    cabal haddock # see https://github.com/haskell/cabal/issues/2198
    cabal test unit-tests --show-details=streaming --test-option=--hide-successes
    cabal test integration-tests --show-details=streaming --test-option=--hide-successes
    cabal test integration-tests2 --show-details=streaming --test-option=--hide-successes
    cabal test solver-quickcheck --show-details=streaming --test-option=--hide-successes \
        --test-option=--quickcheck-tests=1000
    cabal check
    ./dist/setup/setup sdist
    install_from_tarball

}

timed "cabal-install dependencies" cabalinstall_dependencies
timed "cabal-install build" cabalinstall_build

# Check what we got
~/.cabal/bin/cabal --version
