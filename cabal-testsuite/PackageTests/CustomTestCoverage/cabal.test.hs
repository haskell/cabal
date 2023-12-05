import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "test" ["--enable-coverage"]
