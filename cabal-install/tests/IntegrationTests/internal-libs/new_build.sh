. ./common.sh

cabal new-build p || exit 0
exit 1 # expect broken
