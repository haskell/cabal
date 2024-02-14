import Test.Cabal.Prelude

main = cabalTest $ do
    cwd <- testCurrentDir <$> getTestEnv
    buildOut <- do
      cabalWithStdin "init" ["-i", "-p", "app"]
        "2\n\n1\n\n10\n\n\n\n\n\n\n\n\n\n"
      setup "configure" []
      setup' "build" ["app"]

    assertFileDoesContain    (cwd </> "app.cabal")   "1.24"
    assertFileDoesContain    (cwd </> "app.cabal")   "BSD3"
    assertFileDoesContain    (cwd </> "app.cabal")   "Simple"
    assertFileDoesNotContain (cwd </> "app.cabal")   "^>="
    assertFileDoesContain    (cwd </> "app/Main.hs") "This should remain as is!"
    assertOutputContains "Linking" buildOut
