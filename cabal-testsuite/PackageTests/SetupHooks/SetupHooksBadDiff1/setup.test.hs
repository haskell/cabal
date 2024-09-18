import Test.Cabal.Prelude
main = setupTest $ do
  fails $ setup "configure" []
