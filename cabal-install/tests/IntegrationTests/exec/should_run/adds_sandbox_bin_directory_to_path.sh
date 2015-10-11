. ../common.sh

cabal sandbox delete > /dev/null
cabal exec my-executable && die "Unexpectedly found executable"

cabal sandbox init > /dev/null
cabal install > /dev/null

# Execute indirectly via bash to ensure that we go through $PATH
cabal exec sh -- -c my-executable || die "Did not find executable"
