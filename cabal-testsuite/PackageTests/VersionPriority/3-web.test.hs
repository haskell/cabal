import Test.Cabal.Prelude

main = cabalTest . withRepo "repo" . withProjectFile "3-web.project" $ do
    -- To avoid this diff:
    --   -Build profile: -w ghc-9.6.3 -O1
    --   +Build profile: -w ghc-<GHCVER> -O1
    skipIfGhcVersion "== 9.6.3"
    cabal "v2-build" ["--dry-run"]