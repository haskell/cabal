import Test.Cabal.Prelude
main = cabalTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  let fn = tmpdir </> "empty-data-dir-0.list"
  cabal "v2-sdist" ["--ignore-project", "--list-only", "--output-directory", tmpdir]
  -- --list-sources outputs with slashes on posix and backslashes on Windows. 'normalise' converts our needle to the necessary format.
  assertFileDoesContain fn $ normalise "foo.dat"
