import Test.Cabal.Prelude
import System.Directory

-- Test that extra-prog-path in the global cabal config (~/.cabal/config)
-- overrides the path for pkg-config while resolving a dependency on zlib.
-- Specifically, this test emulates the scenario on Windows where pkg-config
-- doesn't exist natively and must be provided via extra-prog-path.
main = cabalTest $ do
    env <- getTestEnv
    let
      testDir = testCurrentDir env
      tmpDir  = testTmpDir     env
      -- Scripts is where we put a bad pkg-config
      scripts = tmpDir </> "scripts"
      -- Scripts-Winpath contains a mock pkg-config that returns good results.
      -- We add this to PATH on Windows before executing the test so we can tell
      -- apart when Cabal uses the override or not.
      scripts_winpath = tmpDir </> "scripts-winpath"

    -------------------------
    -- Workaround for the fact that, on Windows, Cabal will only look for
    -- .exe files to satisfy executable dependencies. So we have to create a
    -- shim pkg-config.exe file in 'script'. This is a thin wrapper (which is
    -- explicitly added to Git) that calls whatever is defined in the .shim
    -- file. In our case, we rewrite the .shim file as below so that the
    -- pkg-config script is executed using sh.
    when isWindows $ do
      mb_sh <- fmap takeDirectory <$> liftIO (findExecutable "sh")
      case mb_sh of
          Nothing -> skip "no sh"
          Just sh -> do
            let escape = concatMap (\c -> case c of '\\' -> "\\\\\\\\"; x -> [x])
            void $ shell "sed" [ "-i", "-e", "s/FINDSH/" <> escape sh <> "/g", escape (scripts </> "pkg-config.shim"), escape (scripts_winpath </> "pkg-config.shim") ]
            void $ shell "sed" [ "-i", "-e", "s/SCRIPTSDIR/" <> escape scripts <> "/g", escape (scripts </> "pkg-config.shim") ]
            void $ shell "sed" [ "-i", "-e", "s/SCRIPTSWINPATHDIR/" <> escape scripts_winpath <> "/g", escape (scripts_winpath </> "pkg-config.shim") ]

    -- End of Windows workaround
    ------------------------------

    -- On Windows, we want it to find the "scripts/pkg-config" (which will
    -- return exit code 1), instead of what we add to the path
    -- ("scripts-winpath/pkg-config", which will return exit code 0). This is
    -- because Windows doesn't have a system pkg-config by default. If we didn't
    -- add a known-to-be-good mock pkg-config in the path, we wouldn't be able
    -- to tell apart from Cabal logs whether it wasn't able to find pkg-config
    -- at all (test fail) or whether the override worked and it found the bad
    -- one but couldn't query it (test success).
    --
    -- On other systems, we want it to find "scripts/pkg-config" (exit code 1)
    -- instead of the system pkg-config (success).
    let wrap_test = if isWindows then addToPath scripts_winpath else id

    -- Add global config override in ~/.cabal/config (in the test environment).
    liftIO $ appendFile (testUserCabalConfigFile env) $
      "\nextra-prog-path: " ++ scripts
    liftIO $ putStrLn $ testUserCabalConfigFile env

    -- On correct behaviour, cabal should fail because it found our exit-code-1
    -- pkg-config through the global extra-prog-path.
    fails $ wrap_test $ cabal "v2-build" ["client"]
