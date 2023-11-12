import Test.Cabal.Prelude
-- Check that preprocessors (hsc2hs) are run
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    setup_build ["--enable-tests", "--enable-benchmarks"]
