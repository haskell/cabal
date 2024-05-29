import Test.Cabal.Prelude

main = cabalTest $ withShorterPathForNewBuildStore $ do
    storeDir <- testStoreDir <$> getTestEnv
    -- The default install method is symlink that may not work on Windows.
    skipIfWindows
    let options = ["--installdir=" ++ storeDir]
    cabalG options "v2-install" []
