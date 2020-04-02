import Test.Cabal.Prelude
main = cabalTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  res <- fails $ cabal' "v2-sdist" ["--ignore-project", "--list-only", "--output-directory", tmpdir]
  assertOutputContains "filepath wildcard './actually-a-directory' does not match any files" res
