import Test.Cabal.Prelude

main = setupAndCabalTest $ recordMode DoNotRecord $ do
  setup "configure" []
  setup "build" []
