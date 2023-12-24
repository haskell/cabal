import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "cabal.project" $ do
    fails $ cabal "v2-build" ["all", "--dry-run"]