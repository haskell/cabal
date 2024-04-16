import Test.Cabal.Prelude
main = setupTest $ do
  setup "configure" []
  fails $ setup "build" []
