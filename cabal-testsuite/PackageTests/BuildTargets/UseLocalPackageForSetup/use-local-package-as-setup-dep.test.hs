import Test.Cabal.Prelude

-- This test case is a simplified version of #4295. There is a local package,
-- pkg-1.0, which has a setup dependency on setup-dep==2.*. The repo contains
-- setup-dep-1.0, and the project contains the newer version, setup-dep-2.0.
-- pkg-1.0 also has a non-setup dependency on setup-dep==1.*.
--
-- The solution to the dependency problem must use the local setup-dep only as a
-- setup dependency for pkg. This means that setup-dep cannot use the same
-- qualifier as pkg, even though they are both build targets of the project.
-- The solution must use --independent-goals to give pkg and setup-dep different
-- qualifiers.
main = cabalTest $ withRepo "repo" $ do
  fails $ cabal "new-build" ["pkg", "--dry-run"]
  cabal "new-build" ["pkg", "--dry-run", "--independent-goals"]
