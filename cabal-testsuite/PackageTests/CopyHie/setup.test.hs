import Test.Cabal.Prelude
import Distribution.Simple.LocalBuildInfo

main = setupAndCabalTest $ withPackageDb $ do
  skipUnlessGhcVersion ">= 8.8"
  setup_install ["hie-local"]
  lbi <- getLocalBuildInfoM
  let installedLibPath = libdir $ absoluteInstallDirs (localPkgDescr lbi) lbi NoCopyDest
  shouldExist $ installedLibPath </> "extra-compilation-artifacts" </> "hie" </> "HieLocal.hie"
