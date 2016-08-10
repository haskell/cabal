. ./common.sh

cabal sandbox init
cabal sandbox add-source p
cabal sandbox add-source q
cabal install p || exit 0
exit 1 # expect broken
