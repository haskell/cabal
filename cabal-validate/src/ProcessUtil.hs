-- | Utilities for running processes and timing them.
module ProcessUtil
  ( timed
  , timedWithCwd
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (ExitCodeException (..), proc, readProcess, runProcess)

import ANSI (SGR (BrightBlue, BrightGreen, BrightRed, Reset), setSGR)
import Cli (Opts (..))
import ClockUtil (diffAbsoluteTime, formatDiffTime, getAbsoluteTime)

-- | Like `timed`, but runs the command in a given directory.
timedWithCwd
  :: Opts
  -- ^ @cabal-validate@ options.
  -> FilePath
  -- ^ Path to run the command in.
  -> FilePath
  -- ^ The command to run.
  -> [String]
  -- ^ Arguments to pass to the command.
  -> IO ()
timedWithCwd opts cdPath command args =
  withCurrentDirectory cdPath (timed opts command args)

-- | Run a command, displaying timing information after it finishes.
--
-- This prints out the command to be executed before it's run, handles hiding
-- or showing output (according to the value of `verbose`), and throws an
-- `ExitCodeException` if the command fails.
timed
  :: Opts
  -- ^ @cabal-validate@ options.
  -> FilePath
  -- ^ The command to run.
  -> [String]
  -- ^ Arguments to pass to the command.
  -> IO ()
timed opts command args = do
  let prettyCommand = displayCommand command args
      process = proc command args

  startTime' <- getAbsoluteTime

  -- TODO: Replace `$HOME` or `opts.cwd` for brevity?
  putStrLn $
    setSGR [BrightBlue]
      <> "$ "
      <> prettyCommand
      <> setSGR [Reset]

  (exitCode, rawStdout, rawStderr) <-
    if verbose opts
      then do
        exitCode <- runProcess process
        pure (exitCode, ByteString.empty, ByteString.empty)
      else readProcess process

  endTime <- getAbsoluteTime

  let duration = diffAbsoluteTime endTime startTime'
      totalDuration = diffAbsoluteTime endTime (startTime opts)

      output = decodeStrip rawStdout <> "\n" <> decodeStrip rawStderr
      linesLimit = 50
      outputLines = T.lines output
      hiddenLines = length outputLines - linesLimit
      tailLines = drop hiddenLines outputLines

  case exitCode of
    ExitSuccess -> do
      unless (verbose opts) $ do
        if hiddenLines <= 0
          then T.putStrLn output
          else
            T.putStrLn $
              "("
                <> T.pack (show hiddenLines)
                <> " lines hidden, use `--verbose` to show)\n"
                <> "...\n"
                <> T.unlines tailLines

      putStrLn $
        setSGR [BrightGreen]
          <> "Finished after "
          <> formatDiffTime duration
          <> ": "
          <> prettyCommand
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGR [Reset]
    ExitFailure exitCode' -> do
      unless (verbose opts) $ do
        T.putStrLn output

      putStrLn $
        setSGR [BrightRed]
          <> "Failed with exit code "
          <> show exitCode'
          <> " after "
          <> formatDiffTime duration
          <> ": "
          <> prettyCommand
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGR [Reset]

      throwIO
        ExitCodeException
          { eceExitCode = exitCode
          , eceProcessConfig = process
          , eceStdout = rawStdout
          , eceStderr = rawStderr
          }

-- | Decode `ByteString` output from a command and strip whitespace at the
-- start and end.
decodeStrip :: ByteString -> Text
decodeStrip = T.strip . T.toStrict . T.decodeUtf8

-- | Escape a shell command to display it to a user.
--
-- TODO: Shell escaping
displayCommand :: String -> [String] -> String
displayCommand command args = command <> " " <> unwords args
