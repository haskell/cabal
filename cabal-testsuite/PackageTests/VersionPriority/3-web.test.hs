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
    -- To avoid this diff:
    --   -Build profile: -w ghc-9.6.3 -O1
    --   +Build profile: -w ghc-<GHCVER> -O1
    skipIfGhcVersion "== 9.6.3"
    testVersionWin "3-web-constraints-import.project"
    testVersionWin "3-web-import-constraints.project"
