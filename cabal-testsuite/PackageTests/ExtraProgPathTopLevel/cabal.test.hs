import Test.Cabal.Prelude

-- Ensure that a top-level extra-prog-path in a project file applies only to
-- local packages.
main = cabalTest $ withRepo "repo" $ do
  env <- getTestEnv
  liftIO $ appendFile (testCurrentDir env </> "cabal.project") $
    "\nextra-prog-path: " ++ testTmpDir env </> "scripts-top"
  cabal "v2-build" ["local"]
