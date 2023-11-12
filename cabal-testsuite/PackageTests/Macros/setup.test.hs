import Test.Cabal.Prelude
-- Test to ensure that setup_macros.h are computed per-component.
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    setup_build []
    runExe "macros-a" []
    runExe "macros-b" []

