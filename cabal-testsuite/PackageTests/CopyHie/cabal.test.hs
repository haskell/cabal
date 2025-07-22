import Test.Cabal.Prelude

main = cabalTest $ withShorterPathForNewBuildStore $ withRepo "repo" $ do
  skipUnlessGhcVersion ">= 8.8"
  cabal "v2-build" ["hie"]
  installedDependencyLibDir <- findDependencyInStore "hie-dependency"
  shouldExist $ installedDependencyLibDir </> "lib" </> "extra-compilation-artifacts" </> "hie" </> "HieDependency.hie"
