import Test.Cabal.Prelude

main = cabalTest $
  withSourceCopyDir "app" $ do
    cwd <- fmap testSourceCopyDir getTestEnv

    buildOut <- withDirectory cwd $ do
      cabalWithStdin "init" ["-i"]
        "2\n\n5\n\n\n2\n\n\n\n\n\n\n\n\n\n"
      setup "configure" []
      setup' "build" ["app"]

    assertFileDoesContain (cwd </> "app.cabal")   "3.0"
    assertFileDoesContain (cwd </> "app.cabal")   "BSD-3-Clause"
    assertFileDoesContain (cwd </> "app.cabal")   "Simple"
    assertFileDoesContain (cwd </> "app/Main.hs") "This should remain as is!"
    assertOutputContains "Linking" buildOut
