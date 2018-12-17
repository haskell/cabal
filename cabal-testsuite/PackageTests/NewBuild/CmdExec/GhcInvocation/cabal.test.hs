import Test.Cabal.Prelude
import System.Directory-- (getDirectoryContents, removeFile)
main = cabalTest $ do
    cabal "v2-build" ["inplace-dep"]
    env <- getTestEnv
    liftIO $ removeEnvFiles $ testSourceDir env -- we don't want existing env files to interfere
    -- Drop the compiled executable into the temporary directory, to avoid cluttering the tree. If compilation succeeds, we've tested what we need to!
    tmpdir <- fmap testTmpDir getTestEnv
    let dest = tmpdir </> "a.out"
    cabal "v2-exec" ["ghc", "--", "Main.hs", "-o", dest]
    -- TODO external (store) deps, once v2-install is working

-- copy-pasted from D.C.CmdClean.
removeEnvFiles :: FilePath -> IO ()
removeEnvFiles dir =
  (mapM_ (removeFile . (dir </>)) . filter ((".ghc.environment" ==) . take 16))
  =<< getDirectoryContents dir
