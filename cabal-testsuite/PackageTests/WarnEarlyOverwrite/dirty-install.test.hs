import Test.Cabal.Prelude

import System.FilePath

main = withShorterPathForNewBuildStore $ \storeDir -> cabalTest $ do
    -- Windows does not natively include a touch command.
    -- SEE: https://stackoverflow.com/questions/30011267/create-an-empty-file-on-the-commandline-in-windows-like-the-linux-touch-command
    skipIfWindows
    let options = ["--store-dir=" ++ storeDir, "--installdir=" ++ storeDir]
    -- Touch the target to see if the warning is made early before the build.
    _ <- runM "touch" [storeDir </> "warn-early-overwrite"] Nothing
    fails $ cabalG options "v2-install" []
    cabalG options "v2-install" ["--overwrite-policy=always"]
