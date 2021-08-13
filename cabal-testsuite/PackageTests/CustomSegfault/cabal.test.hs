import Test.Cabal.Prelude
main = cabalTest $ do
    -- TODO: this test ought to work on Windows too
    skipUnless "not Linux" =<< isLinux
    skipUnlessGhcVersion ">= 7.8"
    fails $ cabal' "v2-build" [] >>= assertOutputContains "SIGSEGV"
