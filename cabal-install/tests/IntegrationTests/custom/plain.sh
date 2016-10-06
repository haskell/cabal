. ./common.sh

# On Travis OSX, Cabal shipped with GHC 7.8 does not work
# with error "setup: /usr/bin/ar: permission denied"; see
# also https://github.com/haskell/cabal/issues/3938
# This is a hack to make the test not run in this case.
if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    require_ghc_ge 710
fi

cd plain
cabal configure 2> log
grep Custom log
cabal build 2> log
grep Custom log
