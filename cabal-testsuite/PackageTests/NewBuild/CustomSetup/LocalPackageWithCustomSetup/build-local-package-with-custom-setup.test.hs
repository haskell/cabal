import Test.Cabal.Prelude

-- The one local package, pkg, has a setup dependency on setup-dep-2.0, which is
-- in the repository.
main = cabalTest $ withShorterPathForNewBuildStore $ do
    skipUnless "no v2-build compatible boot-Cabal" =<< hasNewBuildCompatBootCabal
    withRepo "repo" $ do
      r <- recordMode DoNotRecord $ cabal' "v2-build" ["pkg"]
      -- pkg's setup script should print out a message that it imported from
      -- setup-dep:
      assertOutputContains "pkg Setup.hs: setup-dep-2.0" r
