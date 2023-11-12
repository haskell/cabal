import Test.Cabal.Prelude
-- Test "old build-dep behavior", where we should get the
-- same package dependencies on all targets if setup-version
-- is sufficiently old.
main = setupAndCabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    setup_build []

