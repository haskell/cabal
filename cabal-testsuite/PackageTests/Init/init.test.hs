import Test.Cabal.Prelude

main = cabalTest $
  withSourceCopyDir "app" $ do
    cwd <- fmap testSourceCopyDir getTestEnv

    buildOut <- withDirectory cwd $ do
      cabal "init" ["-n", "--exe", "--application-dir=app", "--main-is=Main.hs"]
      setup "configure" []
      setup' "build" ["app"]

    assertFileDoesContain (cwd </> "app.cabal")   "Simple"
    assertFileDoesContain (cwd </> "app.cabal")   "base ^>="
    assertFileDoesContain (cwd </> "app/Main.hs") "This should remain as is!"
    assertOutputContains "Linking" buildOut
