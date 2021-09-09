import Test.Cabal.Prelude
main = cabalTest $
    cabal "v2-build" ["issue"]
