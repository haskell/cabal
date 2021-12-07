import Test.Cabal.Prelude
import System.Environment (setEnv)
import Distribution.Client.ScriptUtils (getScriptCacheDirectory)

main = cabalTest . void $ do
    res <- cabalWithStdin "v2-repl" ["script.hs"] ":main"
    assertOutputContains "Hello World" res

    env <- getTestEnv
    liftIO $ setEnv "CABAL_DIR" (testCabalDir env)
    cacheDir <- liftIO $ getScriptCacheDirectory "repl:" (testCurrentDir env </> "script.hs")

    shouldExist $ cacheDir </> "fake-package.cabal"
    shouldExist $ cacheDir </> "scriptlocation"
    shouldNotExist $ cacheDir </> "Main.hs"
