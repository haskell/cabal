import Test.Cabal.Prelude
main = setupTest $ do
    skipUnless "no Cabal for GHC" =<< hasCabalForGhc
    setup' "configure" [] >>= assertOutputContains "ThisIsCustomYeah"
    setup' "build"     [] >>= assertOutputContains "ThisIsCustomYeah"
