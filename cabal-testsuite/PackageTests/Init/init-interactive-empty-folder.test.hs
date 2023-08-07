import Test.Cabal.Prelude

main = cabalTest $ do
  tmpDir <- testTmpDir <$> getTestEnv
  withDirectory tmpDir $ do
    res <- cabalWithStdin "init"
                          ["-i"]
                          (replicate 20 '\n') -- Default all the way down.
    assertOutputDoesNotContain "backups will be created" res

