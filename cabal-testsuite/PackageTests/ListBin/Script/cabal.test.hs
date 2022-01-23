import Test.Cabal.Prelude

main = cabalTest . void $ do
    res <- cabal' "list-bin" ["script.hs"]

    env      <- getTestEnv
    cacheDir <- getScriptCacheDirectory $ testCurrentDir env </> "script.hs"
    assertOutputContains cacheDir res
