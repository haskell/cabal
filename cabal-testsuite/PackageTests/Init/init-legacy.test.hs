import Test.Cabal.Prelude

main = cabalTest $ do
    cwd <- fmap testCurrentDir getTestEnv

    buildOut <- do
      cabal "init" ["-n", "--exe", "-p", "app", "--application-dir=app", "--main-is=Main.hs", "--cabal-version=1.24"]
      setup "configure" []
      setup' "build" ["app"]

    assertFileDoesContain    (cwd </> "app.cabal")   "1.24"
    assertFileDoesContain    (cwd </> "app.cabal")   "Simple"
    assertFileDoesNotContain (cwd </> "app.cabal")   "^>="
    assertFileDoesContain    (cwd </> "app/Main.hs") "This should remain as is!"
    assertOutputContains "Linking" buildOut
