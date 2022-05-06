import Test.Cabal.Prelude
main = setupAndCabalTest $
  withPackageDb $ do
    withDirectory "dep" $ setup_install []
    withDirectory "p" $ setup_build []
