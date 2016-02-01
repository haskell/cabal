. ../common.sh

export CABAL_CONFIG=./my-config
cabal user-config init || die "Couldn't create config file"
test -e ./my-config || die "Config file doesn't exist"
