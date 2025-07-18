import Test.Cabal.Prelude

import System.Directory ( doesFileExist )

main = cabalTest $ do
  env <- getTestEnv
  case testPackageDbPath env of
    Nothing -> skip "Cabal-hooks library unavailable."
    Just _pkgdb -> recordMode DoNotRecord $ do
      cabal "v2-build" []
      let setupHooksPath = testCurrentDir env </> "SetupHooks.hs"
      setupHooksExists <- liftIO $ doesFileExist setupHooksPath
      unless setupHooksExists $
        error "Broken test: tried to write to a SetupHooks.hs file that doesn't exist."
      liftIO $ appendFile setupHooksPath "this should fail to compile!"
      -- If this doesn't fail, it's because we didn't re-build.
      fails $ cabal "v2-build" []
