import Test.Cabal.Prelude

main = cabalTest $ withRepo "repo" $ do
    output <- fails $ cabal' "v2-build" ["plain"]
    assertOutputContains "(constraint from cabal.project requires ==0.9)" output
