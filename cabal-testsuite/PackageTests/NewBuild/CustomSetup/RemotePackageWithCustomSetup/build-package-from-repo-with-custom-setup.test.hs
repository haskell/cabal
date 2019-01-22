import Test.Cabal.Prelude

-- The one local package, pkg, has a dependency on remote-pkg-2.0, which has a
-- setup dependency on remote-setup-dep-3.0.
main = withShorterPathForNewBuildStore $ \storeDir ->
  cabalTest $ do

    -- TODO: Debug this failure on Windows.
    skipIf =<< isWindows

    skipUnless =<< hasNewBuildCompatBootCabal
    withRepo "repo" $ do
      r1 <- recordMode DoNotRecord $ cabalG' ["--store-dir=" ++ storeDir] "v2-build" ["pkg:my-exe"]
      -- remote-pkg's setup script should print out a message that it imported from
      -- remote-setup-dep:
      assertOutputContains "remote-pkg Setup.hs: remote-setup-dep-3.0" r1
      withPlan $ do
        r2 <- runPlanExe' "pkg" "my-exe" []
        -- pkg's executable should print a message that it imported from remote-pkg:
        assertOutputContains "pkg Main.hs: remote-pkg-2.0" r2
