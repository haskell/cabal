import Test.Cabal.Prelude

main = cabalTest $
  withSourceCopyDir "app" $ do
    cwd <- fmap testSourceCopyDir getTestEnv
    cabal "init" ["-n", "--exe", "--application-dir=app", "--main-is=Main.hs", "--", cwd]

    assertFileDoesContain (cwd </> "app/Main.hs") "This should remain as is!"
