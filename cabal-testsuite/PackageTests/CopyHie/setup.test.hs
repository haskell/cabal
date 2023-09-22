import Test.Cabal.Prelude

main = setupAndCabalTest $ withPackageDb $ do
  skipUnlessGhcVersion ">= 8.8"
  setup_install ["hie-local"]
  env <- getTestEnv
  shouldExist $ testLibInstallDir env </> "hie-local-0.1.0.0" </> "extra-compilation-artifacts" </> "hie" </> "HieLocal.hie"
