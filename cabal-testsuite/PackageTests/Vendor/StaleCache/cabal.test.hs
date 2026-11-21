import System.Directory (copyFile)
import Test.Cabal.Prelude

-- cabal keeps an index cache in the vendor directory and does not notice when
-- the directory changes under it, as happens after a pull. Running vendor
-- again drops the cache before it plans, so it sees what is in the directory.
main = cabalTest $ withShorterPathForNewBuildStore $ recordMode DoNotRecord $ withRepo "repo" $ do
  env <- getTestEnv
  let vendorDir = testCurrentDir env </> "vendor"

  -- Vendor one of the two dependencies. Planning from the vendored repository
  -- alone then fails, and leaves an index cache that lacks other-lib.
  cabal "v2-vendor" ["my-lib"]
  withProjectFile "cabal.vendored.project" $ do
    fails $ cabal "v2-build" ["--dry-run", "all"]
    shouldExist (vendorDir </> "noindex.cache")

    -- other-lib arrives in the directory without cabal vendor being run.
    liftIO $
      copyFile
        (testRepoDir env </> "other-lib-1.0.tar.gz")
        (vendorDir </> "other-lib-1.0.tar.gz")

    cabal' "v2-vendor" [] >>= assertOutputContains "Vendored 2 packages into vendor"
    cabal "v2-build" ["--offline", "all"]
