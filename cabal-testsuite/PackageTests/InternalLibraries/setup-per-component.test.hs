import Test.Cabal.Prelude
-- No cabal test because per-component is broken for it
main = setupTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    withPackageDb $ do
      withDirectory "p" $ do
        setup_install ["q"]
        setup_install ["p"]
        setup_install ["foo"]
        runInstalledExe "foo" []

