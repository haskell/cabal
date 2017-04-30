import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnless =<< hasNewBuildCompatBootCabal
    cabal "new-build" ["foo"]
    cabal "new-build" ["dep"]
