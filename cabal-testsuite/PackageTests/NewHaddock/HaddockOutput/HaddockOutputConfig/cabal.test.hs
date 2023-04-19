import Control.Monad.IO.Class (MonadIO (..))
import System.Directory (removePathForcibly)
import Test.Cabal.Prelude

-- Test that `cabal haddock --haddock-output-dir` works from the config file.
main = cabalTest . withRepo "repo" $ do
  testDir <- testSourceDir <$> getTestEnv
  let docsDir = testDir </> "docs"
  liftIO (removePathForcibly docsDir)
  r <- cabal' "haddock" ["A"]
  assertFindInFile "A minimal test package for testing haddock." (docsDir </> "index.html")
