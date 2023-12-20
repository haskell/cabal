import Test.Cabal.Prelude

testVersionWin project =
    withProjectFile project $ do
        let log = recordHeader . pure . ((project <> " ") <>)
        log "--version-win not supplied, default"
        cabal "v2-build" ["--dry-run"]
        log "--version-win=shallowest"
        cabal "v2-build" ["--dry-run", "--version-win=shallowest"]
        log "--version-win=latest"
        cabal "v2-build" ["--dry-run", "--version-win=latest"]

main = cabalTest . withRepo "repo" . recordMode RecordMarked $ do
    testVersionWin "2-local-constraints-import.project"
    testVersionWin "2-local-import-constraints.project"