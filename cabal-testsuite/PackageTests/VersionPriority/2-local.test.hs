import Test.Cabal.Prelude

testVersionWin project =
    withProjectFile project $ do
        fails $ cabal "v2-build" ["--dry-run"]

main = cabalTest . withRepo "repo" $ do
    testVersionWin "2-local-constraints-import.project"
    testVersionWin "2-local-import-constraints.project"
