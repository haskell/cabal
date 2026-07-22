import Test.Cabal.Prelude
import Data.Foldable (for_)

-- Toggling @--relative-build-tree@ on or off must not cause a rebuild: it only
-- affects how paths are recorded, not what is compiled.
main = do
  skipIfWindows "build-tree relocation is not verified on Windows"
  cabalTest $ recordMode DoNotRecord $ do
    cabal "build" ["--relative-build-tree"]

    for_ [["--relative-build-tree"], []] $ \flag -> do
      r <- cabal' "build" flag
      assertOutputContains "Up to date" r
      assertOutputDoesNotContain "Compiling" r
