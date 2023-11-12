import Test.Cabal.Prelude
-- Internal library used by public library; it must be installed and
-- registered.
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    withPackageDb $ do
        withDirectory "foolib" $ setup_install []
        withDirectory "fooexe" $ do
            setup_build []
            runExe' "fooexe" []
                >>= assertOutputContains "25"
