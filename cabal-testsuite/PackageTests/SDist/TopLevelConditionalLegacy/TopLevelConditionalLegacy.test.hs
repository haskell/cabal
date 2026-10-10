import Test.Cabal.Prelude
main = setupTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  let fn = tmpdir </> "sources"
  recordMode DoNotRecord $ do
    setup "sdist" ["--list-sources=" ++ fn]
    assertFileDoesNotContain fn "lib/A.hs"
    assertFileDoesContain fn "src/runtime/a.c"
