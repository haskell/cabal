import Test.Cabal.Prelude

main = cabalTest $ do
    skipIfWindows
    withProjectFile "cabal.positive.project" $ do
        cabal "v2-build" ["-v0"]
    withProjectFile "cabal.negative.project" $ do
        fails $ cabal "v2-build" ["-v0"]
