import Test.Cabal.Prelude
-- NB: The --dependency flag is not supported by cabal-install
main = setupTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 9414 $
    withPackageDb $ do
        base_id <- getIPID "base"
        setup_install ["sublib", "--cid", "sublib-0.1-abc"]
        setup_install [ "exe", "--exact-configuration"
                      , "--dependency", "Lib:sublib=sublib-0.1-abc"
                      , "--dependency", "base=" ++ base_id ]
        runExe' "exe" [] >>= assertOutputContains "OK"
