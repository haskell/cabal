import Test.Cabal.Prelude
main = setupAndCabalTest $ do
  withPackageDb $ do
    setup "configure" []
    setup "build" []
    setup "copy" []
