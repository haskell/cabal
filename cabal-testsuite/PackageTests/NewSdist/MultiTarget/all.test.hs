import Test.Cabal.Prelude
main = cabalTest $ withSourceCopy $ do
  cwd <- fmap testCurrentDir getTestEnv
  cabal "v2-sdist" ["all"]
  shouldExist $ cwd </> "dist-newstyle/sdist/a-0.1.tar.gz"
  shouldExist $ cwd </> "dist-newstyle/sdist/b-0.1.tar.gz"
