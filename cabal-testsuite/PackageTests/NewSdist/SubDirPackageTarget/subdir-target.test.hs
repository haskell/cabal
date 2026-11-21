import Test.Cabal.Prelude
main = cabalTest $ do
  cwd <- fmap testCurrentDir getTestEnv
  -- #11385: bare package name typed inside the package directory
  withDirectory "p" $ cabal "v2-sdist" ["p"]
  -- explicit component form selects the containing package
  withDirectory "p" $ cabal "v2-sdist" ["p:lib:p"]
  -- package name from the project root keeps working
  cabal "v2-sdist" ["p"]
  shouldExist $ cwd </> "dist-newstyle/sdist/p-0.1.tar.gz"
