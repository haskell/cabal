import Test.Cabal.Prelude
main = setupTest $ do
  -- No cabal test because per-component is broken with it
  skipUnlessGhcVersion ">= 8.1"
  ghc <- isGhcVersion "== 9.0.2 || == 9.2.* || == 9.4.* || == 9.6.*"
  expectBrokenIf ghc 7987 $
    withPackageDb $
      withDirectory "Includes2" $ do
        let setup_install' args = setup_install_with_docs args
        setup_install' ["mylib", "--cid", "mylib-0.1.0.0"]
        setup_install' ["mysql", "--cid", "mysql-0.1.0.0"]
        setup_install' ["postgresql", "--cid", "postgresql-0.1.0.0"]
        setup_install' ["mylib", "--cid", "mylib-0.1.0.0",
                      "--instantiate-with", "Database=mysql-0.1.0.0:Database.MySQL"]
        setup_install' ["mylib", "--cid", "mylib-0.1.0.0",
                      "--instantiate-with", "Database=postgresql-0.1.0.0:Database.PostgreSQL"]
        setup_install' ["Includes2"]
        setup_install' ["exe"]
        runExe' "exe" [] >>= assertOutputContains "minemysql minepostgresql"
