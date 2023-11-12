import Test.Cabal.Prelude
-- Test that per-component copy works, when only building library
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    withPackageDb $ do
        setup "configure" []
        setup "build" ["lib:p"]
        setup "copy" ["lib:p"]
