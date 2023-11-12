import Test.Cabal.Prelude
main = setupAndCabalTest $ do
  skipUnlessGhcVersion ">= 8.1"
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    skipUnless "no profiling libs" =<< hasProfiledLibraries
    setup "configure" ["--enable-profiling"]
    setup "build" []
