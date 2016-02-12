. ../common.sh

cabal sandbox delete > /dev/null
cabal exec my-executable && die "Unexpectedly found executable"

cabal sandbox init > /dev/null
cabal install > /dev/null

# The library should not be available outside the sandbox
"$GHC_PKG" list | grep -v "my-0.1"

# Execute ghc-pkg inside the sandbox; it should find my-0.1
cabal exec ghc-pkg list | grep "my-0.1"
