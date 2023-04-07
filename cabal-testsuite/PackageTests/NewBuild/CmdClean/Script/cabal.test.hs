import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-build" ["script.hs"]
    cabal' "v2-clean" ["script.hs"]

    env <- getTestEnv
    cacheDir <- getScriptCacheDirectory (testCurrentDir env </> "script.hs")

    shouldDirectoryNotExist cacheDir
    shouldDirectoryNotExist (testDistDir env)
