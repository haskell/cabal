import Test.Cabal.Prelude
import System.Directory (copyFile, removeFile)

main = cabalTest . void $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 8451 $ do
    env <- getTestEnv
    let td = testCurrentDir env

    cabal' "v2-build" ["script.hs"]
    liftIO $ copyFile (td </> "script.hs") (td </> "script2.hs")
    cabal' "v2-build" ["script2.hs"]
    liftIO $ removeFile (td </> "script2.hs")
    cabal' "v2-clean" []

    cacheDir  <- getScriptCacheDirectory (td </> "script.hs")
    cacheDir2 <- getScriptCacheDirectory (td </> "script2.hs")

    shouldDirectoryExist cacheDir
    shouldDirectoryNotExist cacheDir2
