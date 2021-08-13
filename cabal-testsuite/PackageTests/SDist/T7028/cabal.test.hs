import Test.Cabal.Prelude
main = cabalTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  cabal "v2-sdist" ["--ignore-project", "--list-only", "--output-directory", tmpdir, "t7028"]
