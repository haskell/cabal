import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "3-web.project" . recordMode RecordMarked $ do
    -- To avoid this diff:
    --   -Build profile: -w ghc-9.6.3 -O1
    --   +Build profile: -w ghc-<GHCVER> -O1
    skipIfGhcVersion "== 9.6.3"
    let log = recordHeader . pure
    log "--version-win not supplied, default"
    cabal "v2-build" ["--dry-run"]
    log "--version-win=shallowest"
    cabal "v2-build" ["--dry-run", "--version-win=shallowest"]
    log "--version-win=latest"
    cabal "v2-build" ["--dry-run", "--version-win=latest"]
