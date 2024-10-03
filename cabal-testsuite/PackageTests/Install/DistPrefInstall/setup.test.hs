import Test.Cabal.Prelude

main = setupTest $ recordMode DoNotRecord $ withPackageDb $ do
    setup "configure" []
    setup "build" []
    setup "copy" []
    setup "install" []
    setup "sdist" []
