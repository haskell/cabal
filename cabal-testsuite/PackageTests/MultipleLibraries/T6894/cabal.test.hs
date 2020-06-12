import Test.Cabal.Prelude
main = cabalTest $
    fails $ cabal "v2-build" ["issue"]
