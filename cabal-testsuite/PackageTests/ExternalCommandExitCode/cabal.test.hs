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
import System.FilePath
import System.Exit

main = do
  cabalTest $ do
    res <- cabalWithStdin "v2-build" ["all"] ""
    exe_path <- withPlan $ planExePath "setup-test" "cabal-aaaa"
    addToPath (takeDirectory exe_path) $ do
      -- Test that the thing works at all
      res <- fails $ cabal_raw_action ["aaaa"] (\h -> () <$ Process.waitForProcess h)
      -- Check the exit code is the one returned by subcommand
      unless (resultExitCode res == ExitFailure 99) (assertFailure $ "Incorrect exit code: " ++ show (resultExitCode res))


cabal_raw_action :: [String] -> (Process.ProcessHandle -> IO ()) -> TestM Result
cabal_raw_action args action = do
    configured_prog <- requireProgramM cabalProgram
    env <- getTestEnv
    r <- liftIO $ runAction (testVerbosity env)
                 (Just $ testCurrentDir env)
                 (testEnvironment env)
                 (programPath configured_prog)
                 args
                 Nothing
                 action
    recordLog r
    requireSuccess r
