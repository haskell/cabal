import Test.Cabal.Prelude
main = cabalTest $ do
        cabal "v2-build" ["all"]
        cabal "v2-test"  ["all"]
        cabal "v2-bench" ["all"]
