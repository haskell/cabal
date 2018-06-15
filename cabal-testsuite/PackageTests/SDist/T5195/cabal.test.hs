import Test.Cabal.Prelude
main = cabalTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  let fn = tmpdir </> "sources"
  res <- fails $ cabal' "v1-sdist" ["--list-sources=" ++ fn]
  assertOutputContains "filepath wildcard './actually-a-directory' does not match any files" res
