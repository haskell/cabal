import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "1-local.project" $ do
    cabal "v2-build" ["--dry-run"]