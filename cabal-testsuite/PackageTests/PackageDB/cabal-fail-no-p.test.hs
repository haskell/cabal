import Test.Cabal.Prelude
main = cabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    withPackageDb $ do
        withDirectory "p" $
            setup_install []

        withDirectory "q" $ do
            res <- fails $ cabal' "v2-build" []
            assertOutputContains "unknown package: p" res
