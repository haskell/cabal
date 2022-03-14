import Test.Cabal.Prelude

main = cabalTest $
  withSourceCopyDir "app" $ do
    cwd <- fmap testSourceCopyDir getTestEnv

    buildOut <- withDirectory cwd $ do
      cabal "init" ["-n", "--exe", "--application-dir=app", "--main-is=Main.hs", "--cabal-version=1.24"]
      setup "configure" []
      setup' "build" ["app"]

    assertFileDoesContain    (cwd </> "app.cabal")   "1.24"
    assertFileDoesContain    (cwd </> "app.cabal")   "Simple"
    assertFileDoesNotContain (cwd </> "app.cabal")   "^>="
    assertFileDoesContain    (cwd </> "app/Main.hs") "This should remain as is!"
    assertOutputContains "Linking" buildOut
