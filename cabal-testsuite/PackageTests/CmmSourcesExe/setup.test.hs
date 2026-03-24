import Test.Cabal.Prelude

main = setupTest $ do
    setup "configure" []
    res <- setup' "build" []
    assertOutputContains "= Post common block elimination =" res
