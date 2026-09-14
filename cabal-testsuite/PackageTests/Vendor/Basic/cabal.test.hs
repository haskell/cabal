import System.Directory (removeDirectoryRecursive, removeFile)
import Test.Cabal.Prelude

-- Vendor the dependencies of a project into a file+noindex repository, then
-- build the project offline from that repository alone.
main = cabalTest $ withShorterPathForNewBuildStore $ withRepo "repo" $ do
  env <- getTestEnv
  let cwd = testCurrentDir env
      vendorDir = cwd </> "vendor"

  -- A dry run writes nothing.
  cabal "v2-vendor" ["--dry-run"]
  shouldDirectoryNotExist vendorDir

  -- Only package names of source dependencies are accepted.
  fails $ cabal "v2-vendor" ["not-a-dep"]
  fails $ cabal "v2-vendor" ["main"]
  fails $ cabal "v2-vendor" ["lib:my-lib"]

  r <- cabal' "v2-vendor" []
  shouldExist (vendorDir </> "my-lib-1.0.tar.gz")
  shouldExist (vendorDir </> "my-lib-1.0.cabal")
  shouldNotExist (vendorDir </> "noindex.cache")
  assertOutputContains "url: file+noindex:vendor" r
  assertOutputContains "active-repositories: vendored" r

  -- Take away the original repository so that only the vendored one can
  -- satisfy the build.
  liftIO $ removeDirectoryRecursive (testRepoDir env)
  withProjectFile "cabal.vendored.project" $ do
    cabal "v2-build" ["--offline", "all"]
    -- Vendoring again from the vendored repository is a no-op.
    cabal "v2-vendor" []
    shouldExist (vendorDir </> "my-lib-1.0.tar.gz")
