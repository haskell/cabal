import Test.Cabal.Prelude
-- https://github.com/haskell/cabal/issues/1919
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    withPackageDb $ do
        setup_install []
        setup "haddock" []
