import Test.Cabal.Prelude
main = cabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    skipUnlessGhcVersion ">= 7.10"
    setup "configure" []
    setup "build" []
