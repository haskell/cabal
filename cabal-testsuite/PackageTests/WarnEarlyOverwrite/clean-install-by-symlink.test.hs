import Test.Cabal.Prelude

main = cabalTest $ expectBrokenIfWindows 10180 $ withShorterPathForNewBuildStore $ do
    storeDir <- testStoreDir <$> getTestEnv
    let options = ["--installdir=" ++ storeDir]
    cabalG options "v2-install" []
