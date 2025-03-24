import Test.Cabal.Prelude

main = setupAndCabalTest $ do
    setup_build ["--enable-tests"]
    setup "test" ["--show-details=streaming"]
