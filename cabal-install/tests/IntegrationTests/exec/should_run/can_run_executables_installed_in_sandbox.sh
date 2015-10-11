. ../common.sh

cabal sandbox delete > /dev/null
cabal exec my-executable && die "Unexpectedly found executable"

cabal sandbox init > /dev/null
cabal install > /dev/null

cabal exec my-executable || die "Did not find executable"
