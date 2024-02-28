import Control.Monad.IO.Class (MonadIO (..))
import System.Directory (removePathForcibly)
import Test.Cabal.Prelude

-- Test that `cabal haddock --haddock-output-dir` works from the command line.
main = cabalTest . withRepo "repo" $ do
  testDir <- testCurrentDir <$> getTestEnv
  let docsDir = testDir </> "docs"
  liftIO (removePathForcibly docsDir)
  r <- cabal' "haddock" ["--haddock-output-dir=docs", "A"]
  assertFindInFile "A minimal test package for testing haddock." (docsDir </> "index.html")
