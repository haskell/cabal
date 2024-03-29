import Test.Cabal.Prelude
-- Test that reexported modules build correctly
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 7.9"
    withPackageDb $ do
        withDirectory "p" $ setup_install []
        withDirectory "q" $ setup_build []
