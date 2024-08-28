import Test.Cabal.Prelude
import System.Directory

main = cabalTest $ do
  tmpDir <- testCurrentDir <$> getTestEnv
  liftIO $ createDirectory (tmpDir </> "empty")
  withDirectory (tmpDir </> "empty") $ do
    res <- cabalWithStdin "init"
                          ["-i"]
                          (replicate 20 '\n') -- Default all the way down.
    assertOutputDoesNotContain "backups will be created" res

