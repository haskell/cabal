import Test.Cabal.Prelude
import System.Directory

main = cabalTest $ do
    cwd <- fmap testCurrentDir getTestEnv

    buildOut <- do
      cabal "init" ["-n", "--exe", "-p", "app", "--application-dir=app", "--main-is=Main.hs"]
      setup "configure" []
      setup' "build" ["app"]

    assertFileDoesContain (cwd </> "app.cabal")   "Simple"
    assertFileDoesContain (cwd </> "app.cabal")   "base ^>="
    assertFileDoesContain (cwd </> "app/Main.hs") "This should remain as is!"
    assertOutputContains "Linking" buildOut
