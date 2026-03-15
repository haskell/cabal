import Test.Cabal.Prelude

import System.Directory
import System.FilePath

main = setupTest . recordMode DoNotRecord $ do
    workDir <- fmap testWorkDir getTestEnv
    setup "configure" []
    setup "build" []
    liftIO $ removePathForcibly $ workDir </> "work" </> "dist" </> "build"
    setup "haddock" []
