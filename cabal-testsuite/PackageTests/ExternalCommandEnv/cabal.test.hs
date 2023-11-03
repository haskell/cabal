import Test.Cabal.Prelude
import qualified System.Process as Process
import Control.Concurrent (threadDelay)
import System.Directory (removeFile)
import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import Data.Maybe
import System.Environment

main = do
  cabalTest $ expectBroken 9402 $ do
    res <- cabalWithStdin "v2-build" ["all"] ""
    exe_path <- withPlan $ planExePath "setup-test" "cabal-aaaa"
    env <- getTestEnv
    path <- liftIO $ getEnv "PATH"
    let newpath = takeDirectory exe_path ++ ":" ++ path
    let new_env = (("OTHER_VAR", Just "is set") : ("PATH", Just newpath) : (testEnvironment env))

    withEnv new_env $ do
      res <- cabal_raw_action ["aaaa"] (\h -> () <$ Process.waitForProcess h)
      assertOutputContains "cabal-install" res
      assertOutputContains "is set" res


cabal_raw_action :: [String] -> (Process.ProcessHandle -> IO ()) -> TestM Result
cabal_raw_action args action = do
    configured_prog <- requireProgramM cabalProgram
    env <- getTestEnv
    r <- liftIO $ runAction (testVerbosity env)
                 (Just (testCurrentDir env))
                 (testEnvironment env)
                 (programPath configured_prog)
                 args
                 Nothing
                 action
    recordLog r
    requireSuccess r
