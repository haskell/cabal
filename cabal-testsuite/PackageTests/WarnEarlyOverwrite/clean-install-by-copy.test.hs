import Test.Cabal.Prelude

main = cabalTest $ withShorterPathForNewBuildStore $ do
    storeDir <- testStoreDir <$> getTestEnv
    let options = ["--installdir=" ++ storeDir]
    -- Use install method copy that should surely work on Windows too but our
    -- path normalization for testing is not good enough yet as can be seen in
    -- this CI failure snippet diff:
    --   -Warning: The directory <GBLTMPDIR>/ghc-<GHCVER>/incoming/new-<RAND><GBLTMPDIR>/ghc-<GHCVER>/<PACKAGE>-<HASH>/bin is not in the system search path.
    --   -Copying 'warn-early-overwrite' to '<GBLTMPDIR>/warn-early-overwrite'
    --   +Warning: The directory <GBLTMPDIR><GHCVER>/incoming/new-2448/Users/RUNNER~1/AppData/Local/Temp/cabal-test-store-28260/ghc-<GHCVER>/WarnEarlyOver_-0.1.0.0-4c19059e06a32b93b2812983631117e77a2d3833/bin is not in the system search path.
    --   +Copying 'warn-early-overwrite' to '<GBLTMPDIR>'
    skipIfWindows
    cabalG options "v2-install" ["--install-method=copy"]
