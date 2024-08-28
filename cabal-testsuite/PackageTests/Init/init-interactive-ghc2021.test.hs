import Test.Cabal.Prelude

main = cabalTest $ do
    env <- getTestEnv
    buildOut <-
      cabalWithStdin "init" ["-i", "-p", "app"]
        "2\n\n5\n\n\n2\n\n\n\n\n\n\n3\n\n"

    assertFileDoesContain (testCurrentDir env </> "app.cabal") "GHC2021"
