. ./common.sh
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
