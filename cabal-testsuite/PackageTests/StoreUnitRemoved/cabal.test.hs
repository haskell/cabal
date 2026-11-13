import Test.Cabal.Prelude

import System.Directory (removeDirectoryRecursive)

-- With the project unchanged, a dependency is planned until it is in the
-- store and planned again once it is removed from there.
main = cabalTest $ recordMode DoNotRecord $ withShorterPathForNewBuildStore $ withRepo "repo" $ do
  dryRun >>= assertOutputContains "remote-1 (lib)"
  cabal "v2-build" ["current"]
  dryRun >>= assertOutputDoesNotContain "remote-1 (lib)"
  findDependencyInStore "remote" >>= liftIO . removeDirectoryRecursive
  dryRun >>= assertOutputContains "remote-1 (lib)"
  where
    dryRun = cabal' "v2-build" ["current", "--dry-run"]
