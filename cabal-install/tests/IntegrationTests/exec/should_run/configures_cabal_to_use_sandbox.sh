. ../common.sh

cabal sandbox delete > /dev/null
cabal exec my-executable && die "Unexpectedly found executable"

cabal sandbox init > /dev/null
cabal install > /dev/null

# The library should not be available outside the sandbox
"$GHC_PKG" list | grep -v "my-0.1"

# When run inside 'cabal-exec' the 'sandbox hc-pkg list' sub-command
# should find the library.
cabal exec sh -- -c 'cd subdir && "$CABAL" sandbox hc-pkg list' | grep "my-0.1"
