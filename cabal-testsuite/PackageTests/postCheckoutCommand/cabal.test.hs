import Test.Cabal.Prelude

main = cabalTest $ do
    withProjectFile "cabal.positive.project" $ do
        cabal "v2-build" []
    withProjectFile "cabal.negative.project" $ do
        fails $ cabal "v2-build" []
