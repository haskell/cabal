import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "2-local.project" . recordMode RecordMarked $ do
    let log = recordHeader . pure
    log "--version-win not supplied, default"
    cabal "v2-build" ["--dry-run"]
    log "--version-win=shallowest"
    cabal "v2-build" ["--dry-run", "--version-win=shallowest"]
    log "--version-win=latest"
    cabal "v2-build" ["--dry-run", "--version-win=latest"]