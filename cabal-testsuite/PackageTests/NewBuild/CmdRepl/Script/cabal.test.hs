import Test.Cabal.Prelude

main = cabalTest . void $ do
    res <- cabalWithStdin "v2-repl" ["script.hs"] ":main"
    assertOutputContains "Hello World" res

    env      <- getTestEnv
    cacheDir <- getScriptCacheDirectory "repl:" (testCurrentDir env </> "script.hs")

    shouldExist $ cacheDir </> "fake-package.cabal"
    shouldExist $ cacheDir </> "scriptlocation"
    shouldNotExist $ cacheDir </> "Main.hs"
