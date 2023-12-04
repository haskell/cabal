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
  cabalTest $ do
    res <- cabalWithStdin "v2-build" ["all"] ""
    exe_path <- withPlan $ planExePath "setup-test" "cabal-aaaa"
    addToPath (takeDirectory exe_path) $ do
      res <- cabal_raw_action ["help", "aaaa"] (\h -> () <$ Process.waitForProcess h)
      assertOutputContains "I am helping with the aaaa command" res


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
