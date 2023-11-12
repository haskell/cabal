import Test.Cabal.Prelude
-- Test that reexported modules build correctly
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    skipUnlessGhcVersion ">= 7.9"
    withPackageDb $ do
        withDirectory "p" $ setup_install ["--cabal-file", "p.cabal"]
        withDirectory "q" $ setup_build []
