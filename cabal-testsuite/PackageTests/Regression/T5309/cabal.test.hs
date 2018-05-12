import Test.Cabal.Prelude
main = cabalTest $ do
        cabal "new-build" ["all"]
        cabal "new-test"  ["all"]
        cabal "new-bench" ["all"]
