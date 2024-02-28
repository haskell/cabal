import Test.Cabal.Prelude
import System.Directory
import System.FilePath
main = cabalTest $ do
  cwd <- fmap testCurrentDir getTestEnv
  liftIO $ createDirectoryIfMissing False $ cwd </> "lists"
  cabal "v2-sdist" ["all", "--list-only", "--output-dir", "lists"]
  assertFindInFile (normalise "a/a.cabal") (cwd </> "lists/a-0.1.list")
  assertFindInFile (normalise "b/b.cabal") (cwd </> "lists/b-0.1.list")
