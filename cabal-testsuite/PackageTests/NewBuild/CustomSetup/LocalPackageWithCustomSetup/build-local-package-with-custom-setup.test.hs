import Test.Cabal.Prelude

-- The one local package, pkg, has a setup dependency on setup-dep-2.0, which is
-- in the repository.
main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $ do
    skipUnless =<< hasNewBuildCompatBootCabal
    withRepo "repo" $ do
      r <- recordMode DoNotRecord $ cabalG' ["--store-dir=" ++ storeDir] "v2-build" ["pkg"]
      -- pkg's setup script should print out a message that it imported from
      -- setup-dep:
      assertOutputContains "pkg Setup.hs: setup-dep-2.0" r
