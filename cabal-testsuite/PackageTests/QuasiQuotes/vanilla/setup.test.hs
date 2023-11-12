import Test.Cabal.Prelude
-- Test building a vanilla library/executable which uses QuasiQuotes
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ setup_build []
