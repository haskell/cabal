import Test.Cabal.Prelude

-- Repeated runs accumulate; --prune removes what the current plan does not
-- need, and nothing else.
main = cabalTest $ withShorterPathForNewBuildStore $ withRepo "repo" $ do
  env <- getTestEnv
  let vendorDir = testCurrentDir env </> "vendor"
      inVendor = (vendorDir </>)

  -- Two runs with different plans leave both versions of my-lib behind.
  cabal "v2-vendor" ["--constraint=my-lib==1.0"]
  shouldExist (inVendor "my-lib-1.0.tar.gz")
  cabal "v2-vendor" []
  shouldExist (inVendor "my-lib-2.0.tar.gz")
  shouldExist (inVendor "my-lib-1.0.tar.gz")

  -- Files that are not package files must survive pruning; an orphan
  -- sidecar must not.
  liftIO $ writeFile (inVendor "preferred-versions") ""
  liftIO $ writeFile (inVendor "notes.txt") "keep me\n"
  liftIO $ writeFile (inVendor "stale-0.1.cabal") "name: stale\nversion: 0.1\n"

  -- A dry run only reports.
  r <- cabal' "v2-vendor" ["--prune", "--dry-run"]
  assertOutputContains "my-lib-1.0.tar.gz" r
  assertOutputContains "stale-0.1.cabal" r
  shouldExist (inVendor "my-lib-1.0.tar.gz")
  shouldExist (inVendor "stale-0.1.cabal")

  -- Pruning goes by the whole plan, not by the packages named on the
  -- command line: my-lib-2.0 stays although only other-lib is vendored here.
  cabal "v2-vendor" ["other-lib", "--prune"]
  shouldNotExist (inVendor "my-lib-1.0.tar.gz")
  shouldNotExist (inVendor "my-lib-1.0.cabal")
  shouldNotExist (inVendor "stale-0.1.cabal")
  shouldExist (inVendor "my-lib-2.0.tar.gz")
  shouldExist (inVendor "my-lib-2.0.cabal")
  shouldExist (inVendor "other-lib-1.0.tar.gz")
  shouldExist (inVendor "preferred-versions")
  shouldExist (inVendor "notes.txt")

  -- Nothing left to prune.
  r' <- cabal' "v2-vendor" ["--prune"]
  assertOutputDoesNotContain "Removed" r'
