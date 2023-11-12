import Test.Cabal.Prelude
-- Check that preprocessors that generate extra C sources are handled
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    setup_build ["--enable-tests", "--enable-benchmarks"]
