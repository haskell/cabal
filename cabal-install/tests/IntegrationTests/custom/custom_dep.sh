. ./common.sh

# On Travis OSX, Cabal shipped with GHC 7.8 does not work
# with error "setup: /usr/bin/ar: permission denied"; see
# also https://github.com/haskell/cabal/issues/3938
# This is a hack to make the test not run in this case.
if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    require_ghc_ge 710
fi

cd custom_dep
cabal sandbox init
cabal sandbox add-source custom
cabal sandbox add-source client
# Some care must be taken here: we want the Setup script
# to build against GHC's bundled Cabal, but passing
# --package-db=clear --package-db=global to cabal is
# insufficient, as these flags are NOT respected when
# building Setup scripts.  Changing HOME to a location
# which definitely does not have a local .cabal
# directory works, the environment variable propagates to GHC.
HOME=`pwd` cabal install client
