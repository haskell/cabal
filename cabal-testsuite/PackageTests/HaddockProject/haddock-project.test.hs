import Test.Cabal.Prelude
import System.Directory (doesFileExist, removeDirectory)

main = cabalTest . withRepo "repo" $ do
    skipUnlessGhcVersion ">= 9.4.0"
    env <- getTestEnv
    let testDir = testCurrentDir env

    cabal "haddock-project" ["all"]
    let asyncHaddocks = "haddocks" </> "async" </> "async.haddock"
    liftIO (doesFileExist (testDir </> asyncHaddocks))
      >>= assertBool ("'" ++ asyncHaddocks ++ "'" ++ "should exist")

