import Test.Cabal.Prelude
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    withPackageDb $ do
      withDirectory "to-install" $ setup_install []
      setup_build []
      r <- runExe' "lemon" []
      assertEqual
        ("executable should have linked with the internal library")
        ("foo foo myLibFunc internal")
        (concatOutput (resultOutput r))
