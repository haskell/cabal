import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnless "depends on `unix` and needs Linux" isLinux
    fails $ cabal' "v2-build" [] >>= assertOutputContains "SIGSEGV"
