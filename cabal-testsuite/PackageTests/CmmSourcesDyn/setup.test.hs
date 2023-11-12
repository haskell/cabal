import Test.Cabal.Prelude

main = setupTest $ do
  skipIf "ghc < 7.8" =<< isGhcVersion "< 7.8"
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    setup "configure" []
    res <- setup' "build" []
    assertOutputContains "= Post common block elimination =" res
