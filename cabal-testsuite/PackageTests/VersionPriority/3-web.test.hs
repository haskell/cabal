import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "3-web.project" $ do
    cabal "v2-build" ["--dry-run"]