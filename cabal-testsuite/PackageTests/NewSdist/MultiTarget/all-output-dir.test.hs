import Test.Cabal.Prelude
import System.Directory
main = cabalTest $ do
  cwd <- fmap testCurrentDir getTestEnv
  liftIO $ createDirectoryIfMissing False $ cwd </> "archives"
  cabal "v2-sdist" ["all", "--output-dir", "archives"]
  shouldNotExist $ cwd </> "dist-newstyle/sdist/a-0.1.tar.gz"
  shouldNotExist $ cwd </> "dist-newstyle/sdist/b-0.1.tar.gz"
  shouldExist $ cwd </> "archives/a-0.1.tar.gz"
  shouldExist $ cwd </> "archives/b-0.1.tar.gz"
