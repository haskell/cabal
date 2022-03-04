import Test.Cabal.Prelude

main = setupTest $ do
    skipIf "ghc < 7.8" =<< isGhcVersion "< 7.8"
    setup "configure" []
    res <- setup' "build" []
    assertOutputContains "= Post common block elimination =" res
