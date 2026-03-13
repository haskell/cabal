import Test.Cabal.Prelude
import System.Directory

-- Test package-local extra-prog-path works.
main = cabalTest $ do
    env <- getTestEnv
    let
      testDir = testCurrentDir env
      tmpDir  = testTmpDir     env
      scripts1 = tmpDir </> "scripts"
      scripts2 = tmpDir </> "scripts2"

    -------------------------
    -- Workaround for the fact that, on Windows, Cabal will only look for
    -- .exe files to satisfy executable dependencs. So we have to create
    -- shim alex.exe files (the good one in 'scripts2', the bad one in 'scripts')
    -- with the logic below.
    when isWindows $ do
      mb_sh <- fmap takeDirectory <$> liftIO (findExecutable "sh")
      case mb_sh of
          Nothing -> skip "no sh"
          Just sh -> do
            let escape = concatMap (\c -> case c of '\\' -> "\\\\\\\\"; x -> [x])
            void $ shell "sed" [ "-i", "-e", "s/FINDSH/" <> escape sh <> "/g", escape (scripts1 </> "alex.shim"), escape (scripts2 </> "alex.shim") ]
            void $ shell "sed" [ "-i", "-e", "s/SCRIPTSDIR/" <> escape scripts1 <> "/g", escape (scripts1 </> "alex.shim") ]
            void $ shell "sed" [ "-i", "-e", "s/SCRIPTS2DIR/" <> escape scripts2 <> "/g", escape (scripts2 </> "alex.shim") ]

    -- End of Windows workarounds
    ------------------------------

    -- Add the 'scripts' directory to PATH, and add the 'scripts2' directory
    -- to extra-prog-path.
    --
    -- This checks that the executables in extra-prog-path take priority over
    -- those in PATH: 'scripts/alex' will fail, while 'scripts2/alex' will succeed.

    liftIO $ appendFile (testDir </> "cabal.project") $
      "\npackage client\n  extra-prog-path:" ++ scripts2
    addToPath scripts1 $ cabal "v2-build" ["client"]
