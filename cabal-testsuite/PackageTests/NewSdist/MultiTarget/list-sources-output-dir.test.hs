import Test.Cabal.Prelude
import System.Directory
main = cabalTest $ withSourceCopy $ do
  cwd <- fmap testCurrentDir getTestEnv
  liftIO $ createDirectoryIfMissing False $ cwd </> "lists"
  cabal "new-sdist" ["all", "--list-only", "--output-dir", "lists"]
  assertFindInFile "a/a.cabal" (cwd </> "lists/a-0.1.list")
  assertFindInFile "b/b.cabal" (cwd </> "lists/b-0.1.list")
