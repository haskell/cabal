
import Test.Cabal.Prelude
-- NB: The --dependency flag is not supported by cabal-install
main = setupTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $ do
    withPackageDb $ do
        base_id <- getIPID "base"
        setup_install ["sublib", "--cid", "sublib-0.1-abc"]
        r <- fails $ setup' "configure"
                   [ "exe", "--exact-configuration"
                   , "--dependency", "base=" ++ base_id ]
        assertOutputContains "sublib" r
