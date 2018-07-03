import Test.Cabal.Prelude
main = cabalTest $ withSourceCopy $ do
  cwd <- fmap testCurrentDir getTestEnv
  cabal "new-sdist" ["a", "b"]
  shouldExist $ cwd </> "dist-newstyle/sdist/a-0.1.tar.gz"
  shouldExist $ cwd </> "dist-newstyle/sdist/b-0.1.tar.gz"
