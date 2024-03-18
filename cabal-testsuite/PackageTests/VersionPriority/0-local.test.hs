import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "0-local.project" $ do
    fails $ cabal "v2-build" ["--dry-run"]
