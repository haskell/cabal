import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipIfAllCabalVersion "< 2.2"
    setup_build ["--enable-tests"]
    fails $ setup "test" []
