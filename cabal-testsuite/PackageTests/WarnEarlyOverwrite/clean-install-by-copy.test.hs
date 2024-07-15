import Test.Cabal.Prelude

main = do
    skipIfWindows "#10180"
    cabalTest $ withShorterPathForNewBuildStore $ do
        storeDir <- testStoreDir <$> getTestEnv
        let options = ["--installdir=" ++ storeDir]
        cabalG options "v2-install" ["--install-method=copy"]
