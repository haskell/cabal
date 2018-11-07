import Test.Cabal.Prelude
main = cabalTest $ withSourceCopy $ do
  cwd <- fmap testCurrentDir getTestEnv
  fails $ cabal "new-sdist" ["a", "b", "a-tests"]
  shouldNotExist $ cwd </> "dist-newstyle/sdist/a-0.1.tar.gz"
  shouldNotExist $ cwd </> "dist-newstyle/sdist/b-0.1.tar.gz"
