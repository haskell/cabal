import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-build" ["script.hs"]

    env      <- getTestEnv
    cacheDir <- getScriptCacheDirectory $ testCurrentDir env </> "script.hs"

    shouldExist $ cacheDir </> "fake-package.cabal"
    shouldExist $ cacheDir </> "scriptlocation"
