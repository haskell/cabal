import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    setup "configure" []
    setup "build" []
