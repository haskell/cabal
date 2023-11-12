import Test.Cabal.Prelude
-- Test that setup properly orders GHC flags passed to GHC (when
-- there are multiple ghc-options fields.)
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    setup_build []
