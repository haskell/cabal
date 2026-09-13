import System.Directory (removeDirectoryRecursive, removeFile)
import Test.Cabal.Prelude

-- Vendor only one of two dependencies; the other keeps coming from the
-- original repository, which stays active with ':rest'.
main = cabalTest $ withShorterPathForNewBuildStore $ withRepo "repo" $ do
  env <- getTestEnv
  let cwd = testCurrentDir env
      vendorDir = cwd </> "vendor"

  r <- cabal' "v2-vendor" ["my-lib"]
  shouldExist (vendorDir </> "my-lib-1.0.tar.gz")
  shouldNotExist (vendorDir </> "other-lib-1.0.tar.gz")
  assertOutputContains "active-repositories: :rest, vendored:override" r

  -- my-lib is now only available from the vendored repository, other-lib
  -- only from the original one; the build needs both.
  liftIO $ removeFile (testRepoDir env </> "my-lib-1.0.tar.gz")
  liftIO $ removeFile (testRepoDir env </> "noindex.cache")
  withProjectFile "cabal.partial.project" $
    cabal "v2-build" ["all"]
