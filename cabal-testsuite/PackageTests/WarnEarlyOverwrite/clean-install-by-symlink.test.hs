import Test.Cabal.Prelude

main = withShorterPathForNewBuildStore $ \storeDir -> cabalTest $ do
    -- The default install method is symlink that may not work on Windows.
    skipIfWindows
    let options = ["--store-dir=" ++ storeDir, "--installdir=" ++ storeDir]
    cabalG options "v2-install" []
