. ./common.sh

cd p
cabal sandbox init
cabal sandbox add-source ../q

cabal install --only-dependencies
cabal run p -v0 | grep -q '^message$' \
    || die "Expected \"message\" in p's output."

sleep 1
# Append to the string that p imports from q and prints:
echo '       ++ " updated"' >> ../q/Q.hs

cabal run p -v0 | grep -q '^message updated$' \
    || die "Expected \"message updated\" in p's output."
