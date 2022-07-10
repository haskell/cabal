import Test.Cabal.Prelude

main = cabalTest $
  withSourceCopyDir "app" $ do
    cwd <- fmap testSourceCopyDir getTestEnv

    buildOut <- withDirectory cwd $ do
      cabalWithStdin "init" ["-i"]
        "2\n\n5\n\n\n2\n\n\n\n\n\n\n\n3\n\n"

    assertFileDoesContain (cwd </> "app.cabal") "GHC2021"
