import Test.Cabal.Prelude

-- Test that "cabal v2-build pkg" builds the local pkg-1.0, which has an exe
-- that prints a unique message. It should not build 1.0 or 2.0 from the
-- repository.
main = cabalTest $ withRepo "repo" $ do
  cabal "v2-build" ["pkg"]
  withPlan $ do
    r <- runPlanExe' "pkg" "my-exe" []
    assertOutputContains "local pkg-1.0" r

  -- cabal shouldn't build a package from the repo, even when given a constraint
  -- that only matches a non-local package.
  r <- fails $ cabal' "v2-build" ["pkg", "-c", "pkg==2.0"]
  assertOutputContains "rejecting: pkg-2.0" r
