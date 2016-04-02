. ./common.sh

cabal sandbox init
cabal sandbox add-source p
cabal install p
