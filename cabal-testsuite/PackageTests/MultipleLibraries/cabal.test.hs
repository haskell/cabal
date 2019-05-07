import Test.Cabal.Prelude
main = cabalTest $
    void $ fails (cabal' "v2-build" ["p"])

