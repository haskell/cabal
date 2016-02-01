. ../common.sh

rm -f ./cabal-config
cabal --config-file=./cabal-config user-config init > /dev/null
cabal --config-file=./cabal-config user-config init
rm -f ./cabal-config
