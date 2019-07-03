import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    withPackageDb $ do
        setup "configure" []
        setup "build" ["myprog"]
        setup "copy" ["myprog"]
