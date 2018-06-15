import Test.Cabal.Prelude
main = cabalTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  let fn = tmpdir </> "sources"
  cabal "v1-sdist" ["--list-sources=" ++ fn]
  -- --list-sources outputs with slashes on posix and backslashes on Windows. 'normalise' converts our needle to the necessary format.
  assertFileDoesContain fn $ normalise "foo.dat"
