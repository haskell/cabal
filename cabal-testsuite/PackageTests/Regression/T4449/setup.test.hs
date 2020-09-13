import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnlessGhcVersion ">= 7.10"
    setup "configure" []
    setup "build" []
