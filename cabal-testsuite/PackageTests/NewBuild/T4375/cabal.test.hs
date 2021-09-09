import Test.Cabal.Prelude
main = withShorterPathForNewBuildStore $ \storeDir ->
  -- TODO: is this test ever run?
  cabalTest $ do
    -- Don't run this test unless the GHC is sufficiently recent
    -- to not ship boot old-time/old-locale
    skipUnlessGhcVersion ">= 7.11"
    -- Don't run this test on GHC 8.2, which ships with Cabal 2.0,
    -- which is not eligible for old-style Custom setup (if
    -- we had the full Hackage index, we'd try it.)
    skipUnlessGhcVersion "< 8.1"
    withRepo "repo" $ do
        cabalG ["--store-dir=" ++ storeDir] "v2-build" ["a"]
