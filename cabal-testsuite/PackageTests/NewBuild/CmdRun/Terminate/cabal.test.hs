import Test.Cabal.Prelude
import qualified System.Process as Process
import Control.Concurrent (threadDelay)
import System.Directory (removeFile)
import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time

{-
This test verifies that 'cabal run' terminates its
child when it is killed. More generally, while we
use the same code path for all child processes, this
ensure that cabal-install cleans up after all children.
(That might change if 'cabal run' is changed to exec(3)
without forking in the future.)
-}

main :: IO ()
main = cabalTest $ do
  skipIfWindows -- test project relies on Posix

  -- timestamped logging to aid with #8416
  let logIO msg = do
        ts <- Time.getCurrentTime
        let tsfmt = Time.formatTime Time.defaultTimeLocale "%H:%M:%S.%q" ts
        putStrLn $ tsfmt <> " [cabal.test] " <> msg
      log = liftIO . logIO

  dir <- fmap testCurrentDir getTestEnv
  let runFile = dir </> "exe.run"
  liftIO $ removeFile runFile `catchNoExist` return ()

  log "about to v2-build"
  cabal_raw_action ["v2-build", "exe"] (\_ -> return ())
  log "about to v2-run"
  r <- fails $ cabal_raw_action ["v2-run", "exe"] $ \cabalHandle -> do
    -- wait for "cabal run" to have started "exe"
    logIO "about to wait for file"
    waitFile total runFile
    -- then kill "cabal run"
    logIO "about to terminate cabal"
    Process.terminateProcess cabalHandle
  log "v2-run done"

  -- "exe" should exit, and should have been interrupted before
  -- finishing its sleep
  assertOutputContains "exiting" r
  assertOutputDoesNotContain "done sleeping" r

  where
    catchNoExist action handle =
      action `catch`
        (\e -> if isDoesNotExistError e then handle else throwIO e)
    waitFile totalWait f
      | totalWait <= 0 = error "waitFile timed out"
      | otherwise      = readFile f `catchNoExist` do
                           threadDelay delta
                           waitFile (totalWait - delta) f
    delta = 50000 -- 0.05s
    total = 10000000 -- 10s

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
