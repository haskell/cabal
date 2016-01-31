. ../common.sh

rm -f ./cabal-config
cabal --config-file=./cabal-config user-config init \
    || die "Couldn't create config file"
cabal --config-file=./cabal-config user-config -f init \
    || die "Couldn't create config file"
test -e ./cabal-config || die "Config file doesn't exist"
rm -f ./cabal-config
