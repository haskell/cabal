import Test.Cabal.Prelude
main = cabalTest $ do
    setup "configure" []
    setup "build" []
