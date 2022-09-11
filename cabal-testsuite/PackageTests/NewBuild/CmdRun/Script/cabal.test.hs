import Test.Cabal.Prelude

main = cabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 8451 $ do
    res <- cabal' "v2-run" ["script.hs"]
    assertOutputContains "Hello World" res

    env      <- getTestEnv
    cacheDir <- getScriptCacheDirectory (testCurrentDir env </> "script.hs")

    shouldExist $ cacheDir </> "fake-package.cabal"
    shouldExist $ cacheDir </> "scriptlocation"
