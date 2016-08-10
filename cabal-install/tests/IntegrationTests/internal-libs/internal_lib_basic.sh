. ./common.sh

cabal sandbox init
cabal sandbox add-source p
cabal install p || exit 0
exit 1 # expect broket
