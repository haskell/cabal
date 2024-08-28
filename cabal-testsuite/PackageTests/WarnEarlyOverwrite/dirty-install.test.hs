import Test.Cabal.Prelude

import System.FilePath

main = do
    skipIfWindows "#10180"
    cabalTest $ withShorterPathForNewBuildStore $ do
        storeDir <- testStoreDir <$> getTestEnv
        let options = ["--installdir=" ++ storeDir]
        -- Touch the target to see if the warning is made early before the build.
        _ <- runM "touch" [ (if isWindows then (<.> "exe") else id)
                          $ storeDir </> "warn-early-overwrite" ] Nothing
        fails $ cabalG options "v2-install" []
        cabalG options "v2-install" ["--overwrite-policy=always"]
