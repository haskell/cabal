import Test.Cabal.Prelude

main = cabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 8451 $ do
    cabal "v2-test"
      [ "--test-option=1"
      , "--test-options=\"2 3\""
      , "--test-option=4"
      , "--test-options=\"5 6\""
      ]
