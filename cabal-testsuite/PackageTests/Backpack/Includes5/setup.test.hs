import Test.Cabal.Prelude
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    skipUnlessGhcVersion ">= 8.1"
    setup "configure" []
    r <- fails $ setup' "build" []
    assertOutputContains "Foobar" r
    assertRegex
      "error should be about not being able to find a module"
      "Could not (find|load) module"
      r
    return ()
