import Test.Cabal.Prelude

main = setupAndCabalTest $ withPackageDb $ do
  setup_install ["hie-local"]
  env <- getTestEnv
  shouldExist $ testLibInstallDir env </> "hie-local-0.1.0.0" </> "HieLocal.hie"
