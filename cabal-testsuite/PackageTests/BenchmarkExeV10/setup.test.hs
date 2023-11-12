import Test.Cabal.Prelude
-- Test if exitcode-stdio-1.0 benchmark builds correctly
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    setup_build ["--enable-benchmarks"]
