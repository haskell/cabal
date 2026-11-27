import Test.Cabal.Prelude

-- #9136: when a package is added to a file+noindex repository after cabal has
-- written the repository's cache, cabal should notice it without the cache
-- having to be deleted by hand.
main = cabalTest $ withRepo "repo" $ do
  -- The repository only has greet-0.1 at this point, and this writes the cache.
  cabal "v2-build" ["app"]

  -- Add greet-0.2 to the repository.
  env <- getTestEnv
  (testCurrentDir env </> "new-versions" </> "greet-0.2")
    `archiveTo` (testRepoDir env </> "greet-0.2.tar.gz")

  -- Before the fix, the stale cache only listed greet-0.1, so this failed to
  -- find a version of greet satisfying the constraint.
  cabal "v2-build" ["app", "--constraint=greet >= 0.2"]
