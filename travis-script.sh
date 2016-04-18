#!/usr/bin/env bash
set -ev

# Initial working directory: base directory of Git repository

# We depend on parsec nowadays, which isn't distributed with GHC <8.0
if [ "$PARSEC_BUNDLED" != "YES" ]; then
    cabal install parsec
fi

# ---------------------------------------------------------------------
# Cabal
# ---------------------------------------------------------------------

cd Cabal

# Test if gen-extra-source-files.sh was run recently enough
./misc/gen-extra-source-files.sh Cabal.cabal
./misc/travis-diff-files.sh

cd ../cabal-install
../Cabal/misc/gen-extra-source-files.sh cabal-install.cabal
../Cabal/misc/travis-diff-files.sh

cd ../Cabal

# Build the setup script in the same way that cabal-install would:
mkdir -p ./dist/setup
cp Setup.hs ./dist/setup/setup.hs
ghc --make \
    -odir ./dist/setup -hidir ./dist/setup -i -i. \
    ./dist/setup/setup.hs -o ./dist/setup/setup \
    -Wall -Werror -threaded

# Install test dependencies only after setup is built
cabal install --only-dependencies --enable-tests --enable-benchmarks
./dist/setup/setup configure \
    --user --ghc-option=-Werror --enable-tests --enable-benchmarks \
    -v2 # -v2 provides useful information for debugging

# Build all libraries and executables (including tests/benchmarks)
./dist/setup/setup build
./dist/setup/setup haddock # see https://github.com/haskell/cabal/issues/2198
./dist/setup/setup test --show-details=streaming --test-option=--hide-successes

# Redo the package tests with different versions of GHC
if [ "$TEST_OLDER" == "YES" ]; then
    CABAL_PACKAGETESTS_WITH_GHC=/opt/ghc/7.0.4/bin/ghc \
        ./dist/setup/setup test package-tests --show-details=streaming
    CABAL_PACKAGETESTS_WITH_GHC=/opt/ghc/7.2.2/bin/ghc \
        ./dist/setup/setup test package-tests --show-details=streaming
fi

cabal check
cabal sdist   # tests that a source-distribution can be generated

# The following scriptlet checks that the resulting source distribution can be
# built & installed.
function install_from_tarball {
   export SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}') ;
   if [ -f "dist/$SRC_TGZ" ]; then
      cabal install -j1 "dist/$SRC_TGZ" -v2;
   else
      echo "expected 'dist/$SRC_TGZ' not found";
      exit 1;
   fi
}

install_from_tarball

# ---------------------------------------------------------------------
# cabal-install
# ---------------------------------------------------------------------

cd ../cabal-install

cabal install happy
cabal install --only-dependencies --enable-tests --enable-benchmarks --allow-newer=tasty-expected-failure:base
cabal configure \
    --user --ghc-option=-Werror --enable-tests --enable-benchmarks \
    -v2 # -v2 provides useful information for debugging
cabal build
cabal haddock # see https://github.com/haskell/cabal/issues/2198
cabal test unit-tests --show-details=streaming --test-option=--hide-successes
cabal test integration-tests --show-details=streaming --test-option=--hide-successes
cabal check
./dist/setup/setup sdist
install_from_tarball

# Check what we got
$HOME/.cabal/bin/cabal --version
