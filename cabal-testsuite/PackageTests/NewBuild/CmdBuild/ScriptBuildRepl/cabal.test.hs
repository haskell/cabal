import Test.Cabal.Prelude

main = cabalTest . void $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 8451 $ do
    cabal' "v2-build" ["script.hs"]
    cabalWithStdin "v2-repl" ["script.hs"] ""
