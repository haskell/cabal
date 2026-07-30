import Test.Cabal.Prelude
import Data.Foldable (for_)

-- Toggling @--enable-relocatable@ on or off must not cause a rebuild
main = do
  skipIfWindows "does not support relocatable builds"
  cabalTest $ recordMode DoNotRecord $ do
    cabal "build" ["--enable-relocatable"]

    for_ [["--enable-relocatable"], []] $ \flag -> do
      r <- cabal' "build" flag
      assertOutputContains "Up to date" r
      assertOutputDoesNotContain "Compiling" r
