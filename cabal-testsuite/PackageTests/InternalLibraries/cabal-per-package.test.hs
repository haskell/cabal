import Test.Cabal.Prelude
main = cabalTest $ do
    fails $ cabal "v2-build" ["--disable-per-component", "p"]
