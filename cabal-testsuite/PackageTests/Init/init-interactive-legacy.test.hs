import Test.Cabal.Prelude

main = cabalTest $
  withSourceCopyDir "app" $ do
    cwd <- fmap testSourceCopyDir getTestEnv

    buildOut <- withDirectory cwd $ do
      cabalWithStdin "init" ["-i"]
        "2\n\n1\n\n\n10\n\n\n\n\n\n\n\n\n\n"
      setup "configure" []
      setup' "build" ["app"]

    assertFileDoesContain    (cwd </> "app.cabal")   "1.24"
    assertFileDoesContain    (cwd </> "app.cabal")   "BSD3"
    assertFileDoesContain    (cwd </> "app.cabal")   "Simple"
    assertFileDoesNotContain (cwd </> "app.cabal")   "^>="
    assertFileDoesContain    (cwd </> "app/Main.hs") "This should remain as is!"
    assertOutputContains "Linking" buildOut
