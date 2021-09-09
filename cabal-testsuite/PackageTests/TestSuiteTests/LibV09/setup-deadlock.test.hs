import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless "no Cabal for GHC" =<< hasCabalForGhc
    setup_build ["--enable-tests"]
    fails $ setup "test" []
