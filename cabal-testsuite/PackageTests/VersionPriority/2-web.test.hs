import Test.Cabal.Prelude

testVersionWin project =
    withProjectFile project $ do
        fails $ cabal "v2-build" ["--dry-run"]

main = cabalTest . withRepo "repo" $ do
    -- To avoid this diff:
    --   -Build profile: -w ghc-9.6.3 -O1
    --   +Build profile: -w ghc-<GHCVER> -O1
    skipIfGhcVersion "== 9.6.3"
    testVersionWin "2-web-constraints-import.project"
    testVersionWin "2-web-import-constraints.project"
