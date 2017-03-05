import Test.Cabal.Prelude
main = cabalTest $ do
    withRepo "repo" $ do
        cabal "new-build" ["a"]
