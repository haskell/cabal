import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "v2-build" ["lemon"]
