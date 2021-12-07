import Test.Cabal.Prelude
import System.Directory (copyFile, removeFile)
import System.Environment (setEnv)
import Distribution.Client.ScriptUtils (getScriptCacheDirectory)

main = cabalTest . void $ do
    env <- getTestEnv
    let td = testCurrentDir env

    cabal' "v2-build" ["script.hs"]
    liftIO $ copyFile (td </> "script.hs") (td </> "script2.hs")
    cabal' "v2-build" ["script2.hs"]
    liftIO $ removeFile (td </> "script2.hs")
    cabal' "v2-clean" []

    liftIO $ setEnv "CABAL_DIR" (testCabalDir env)
    cacheDir  <- liftIO $ getScriptCacheDirectory "" (testCurrentDir env </> "script.hs")
    cacheDir2 <- liftIO $ getScriptCacheDirectory "" (testCurrentDir env </> "script2.hs")

    shouldDirectoryExist cacheDir
    shouldDirectoryNotExist cacheDir2
