import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "0-local.project" . recordMode RecordMarked $ do
    let log = recordHeader . pure
    log "--version-win not supplied, default"
    fails $ cabal "v2-build" ["--dry-run"]
    log "--version-win=shallowest"
    fails $ cabal "v2-build" ["--dry-run", "--version-win=shallowest"]
    log "--version-win=latest"
    cabal "v2-build" ["--dry-run", "--version-win=latest"]