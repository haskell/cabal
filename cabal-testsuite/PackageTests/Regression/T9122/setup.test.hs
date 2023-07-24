import Test.Cabal.Prelude

main = setupAndCabalTest $ withPackageDb $ do
  withDirectory "p" $ setup_install []
  withDirectory "q" $ setup "configure" []
