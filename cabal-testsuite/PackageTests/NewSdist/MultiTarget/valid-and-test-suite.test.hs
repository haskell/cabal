import Test.Cabal.Prelude
import System.Directory (createDirectoryIfMissing)
import System.FilePath (normalise)
main = cabalTest $ do
  cwd <- fmap testCurrentDir getTestEnv
  liftIO $ createDirectoryIfMissing False $ cwd </> "lists"
  cabal "v2-sdist" ["a", "b", "a-tests"]
  shouldExist $ cwd </> "dist-newstyle/sdist/a-0.1.tar.gz"
  shouldExist $ cwd </> "dist-newstyle/sdist/b-0.1.tar.gz"

  -- Duplicate selectors resolving to the same package must yield a single
  -- package, not two copies of the archive (#11385).  With --list-only the
  -- deduped package is written to a single .list file.
  cabal "v2-sdist" ["a", "a-tests", "--list-only", "--output-dir", "lists"]
  assertFindInFile (normalise "a/a.cabal") (cwd </> "lists/a-0.1.list")
