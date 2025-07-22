import Test.Cabal.Prelude
import System.FilePath
main = cabalTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    let cur_dir = testCurrentDir env
    -- Set GHC_ENVIRONMENT to a file which contains garbage
    withEnv [("GHC_ENVIRONMENT", Just (cur_dir </> "ghc.environment"))] $ do
        cabal "build" ["all"]
