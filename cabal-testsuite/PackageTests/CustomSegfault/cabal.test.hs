import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnless "depends on `unix` and needs Linux" isLinux
    skipUnlessGhcVersion ">= 7.8"
    fails $ cabal' "v2-build" [] >>= assertOutputContains "SIGSEGV"
