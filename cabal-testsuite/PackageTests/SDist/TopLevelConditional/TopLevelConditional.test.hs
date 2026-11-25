import Test.Cabal.Prelude
main = setupTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  let fn = tmpdir </> "sources"
  setup "sdist" ["--list-sources=" ++ fn]
  assertFileDoesContain fn "src/runtime/a.c"
  assertFileDoesContain fn "lib/A.hs"
  assertFileDoesContain fn "changelog"
