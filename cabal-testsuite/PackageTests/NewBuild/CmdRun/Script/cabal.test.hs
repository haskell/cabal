import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
    res <- cabal' "v2-run" ["script.hs"]
    assertOutputContains "Hello World" res

    env      <- getTestEnv
    cacheDir <- getScriptCacheDirectory (testCurrentDir env </> "script.hs")

    shouldExist $ cacheDir </> "fake-package.cabal"
    shouldExist $ cacheDir </> "scriptlocation"
