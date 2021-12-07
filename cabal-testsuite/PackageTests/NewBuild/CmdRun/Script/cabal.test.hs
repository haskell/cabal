import Test.Cabal.Prelude
import System.Environment (setEnv)
import Distribution.Client.ScriptUtils (getScriptCacheDirectory)

main = cabalTest $ do
    res <- cabal' "v2-run" ["script.hs"]
    assertOutputContains "Hello World" res

    env <- getTestEnv
    liftIO $ setEnv "CABAL_DIR" (testCabalDir env)
    cacheDir <- liftIO $ getScriptCacheDirectory "" (testCurrentDir env </> "script.hs")

    shouldExist $ cacheDir </> "fake-package.cabal"
    shouldExist $ cacheDir </> "Main.hs"
    shouldExist $ cacheDir </> "scriptlocation"
