import Test.Cabal.Prelude
main = setupTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  let fn = tmpdir </> "sources"
  setup "sdist" ["--list-sources=" ++ fn]
  assertFileDoesContain fn "data/blah/a.dat"
  assertFileDoesContain fn "extra-src/blah/a.html"
  assertFileDoesContain fn "extra-doc/blah/a.tex"
