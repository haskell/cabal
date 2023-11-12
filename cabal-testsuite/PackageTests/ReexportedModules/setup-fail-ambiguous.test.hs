import Test.Cabal.Prelude
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    skipUnlessGhcVersion ">= 7.9"
    withPackageDb $ do
        withDirectory "containers-dupe" $
            setup_install []
        withDirectory "p" $ do
            r <- fails $ setup' "configure" ["--cabal-file", "p.cabal.fail-ambiguous"]
            assertOutputContains "Data.Map" r
