import Test.Cabal.Prelude

main = cabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 8451 $ do
    -- script is called "s.hs" to avoid Windows long path issue in CI
    res <- cabal' "v2-run" ["s.hs"]
    assertOutputContains "Hello World" res
