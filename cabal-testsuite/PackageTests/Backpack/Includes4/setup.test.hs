import Test.Cabal.Prelude
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    skipUnlessGhcVersion ">= 8.1"
    withPackageDb $ do
      setup_install []
      runExe' "exe" [] >>= assertOutputContains "A (B (A (B"
