import Test.Cabal.Prelude
main = cabalTest $ withSourceCopy $ do
  cwd <- fmap testCurrentDir getTestEnv
  fails $ cabal "v2-sdist" ["a", "base"]
  shouldNotExist $ cwd </> "dist-newstyle/sdist/a-0.1.tar.gz"
