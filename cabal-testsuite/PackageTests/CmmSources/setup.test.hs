import Test.Cabal.Prelude

main = setupTest $ do
    skipIf =<< ghcVersionIs (< mkVersion [7,8])
    setup "configure" []
    res <- setup' "build" []
    assertOutputContains "= Post common block elimination =" res
