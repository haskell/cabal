import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    setup_build ["--enable-tests"]
    fails $ setup "test" []
