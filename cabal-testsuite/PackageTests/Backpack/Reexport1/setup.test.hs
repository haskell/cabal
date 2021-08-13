import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    withPackageDb $ do
      withDirectory "p" $ setup_install_with_docs []
      withDirectory "q" $ do
        setup_build []
        setup "haddock" []
