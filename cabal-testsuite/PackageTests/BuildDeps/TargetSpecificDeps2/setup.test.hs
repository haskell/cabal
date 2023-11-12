import Test.Cabal.Prelude
-- This is a control on ../TargetSpecificDeps1/setup.test.hs; it should
-- succeed.
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    setup_build []
