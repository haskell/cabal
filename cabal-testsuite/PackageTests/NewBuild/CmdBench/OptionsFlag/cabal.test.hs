import Test.Cabal.Prelude

main = cabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 8451 $ do
    cabal "v2-bench"
      [ "--benchmark-option=1"
      , "--benchmark-options=\"2 3\""
      , "--benchmark-option=4"
      , "--benchmark-options=\"5 6\""
      ]
