import System.FilePath (normalise)
import Test.Cabal.Prelude
main = setupTest $ do
  tmpdir <- fmap testTmpDir getTestEnv
  let fn = tmpdir </> "sources"
  setup "sdist" ["--list-sources=" ++ fn]
  -- --list-sources outputs with slashes on posix and backslashes on Windows. 'normalise' converts our needle to the necessary format.
  assertFileDoesContain fn $ normalise "data/blah/a.dat"
  assertFileDoesContain fn $ normalise "extra-src/blah/a.html"
  assertFileDoesContain fn $ normalise "extra-doc/blah/a.tex"
