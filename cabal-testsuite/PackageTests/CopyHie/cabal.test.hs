import Test.Cabal.Prelude

main = withShorterPathForNewBuildStore $ \storeDir -> cabalTest $ withRepo "repo" $ do
  skipUnlessGhcVersion ">= 8.8"
  cabalG ["--store-dir=" ++ storeDir] "v2-build" ["hie"]
  liftIO $ do
    installedDependencyLibDir <- findDependencyInStore storeDir "hie-dependency"
    shouldExist $ installedDependencyLibDir </> "lib" </> "extra-compilation-artifacts" </> "hie" </> "HieDependency.hie"
