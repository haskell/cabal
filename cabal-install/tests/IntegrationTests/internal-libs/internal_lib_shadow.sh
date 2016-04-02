. ./common.sh

cabal sandbox init
cabal sandbox add-source p
cabal sandbox add-source q
cabal install p
