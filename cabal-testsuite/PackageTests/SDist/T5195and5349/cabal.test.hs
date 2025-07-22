import Test.Cabal.Prelude
main = cabalTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  cabal' "v2-sdist" ["--list-only", "--output-directory", tmpdir]
  return ()
