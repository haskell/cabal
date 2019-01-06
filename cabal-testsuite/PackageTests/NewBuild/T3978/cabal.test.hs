import Test.Cabal.Prelude
main = cabalTest $ do
    fails $ cabal "v2-build" ["q"]
