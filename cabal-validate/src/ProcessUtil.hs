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
import System.Console.ANSI
  ( Color (Blue, Green, Red)
  , ColorIntensity (Vivid)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor)
  , setSGRCode
  )
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (ExitCodeException (..), proc, readProcess, runProcess)

import Cli (Opts (..))
import ClockUtil (diffAbsoluteTime, formatDiffTime, getAbsoluteTime)

timedWithCwd :: Opts -> FilePath -> String -> [String] -> IO ()
timedWithCwd opts cdPath command args =
  withCurrentDirectory cdPath (timed opts command args)

timed :: Opts -> String -> [String] -> IO ()
timed opts command args = do
  let prettyCommand = displayCommand command args
      process = proc command args

  startTime' <- getAbsoluteTime

  -- TODO: Replace `$HOME` or `opts.cwd` for brevity?
  putStrLn $
    setSGRCode [SetColor Foreground Vivid Blue]
      <> "$ "
      <> prettyCommand
      <> setSGRCode [Reset]

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
        setSGRCode [SetColor Foreground Vivid Green]
          <> "Finished after "
          <> formatDiffTime duration
          <> ": "
          <> prettyCommand
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGRCode [Reset]
    ExitFailure exitCode' -> do
      unless (verbose opts) $ do
        T.putStrLn output

      putStrLn $
        setSGRCode [SetColor Foreground Vivid Red]
          <> "Failed with exit code "
          <> show exitCode'
          <> " after "
          <> formatDiffTime duration
          <> ": "
          <> prettyCommand
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGRCode [Reset]

      throwIO
        ExitCodeException
          { eceExitCode = exitCode
          , eceProcessConfig = process
          , eceStdout = rawStdout
          , eceStderr = rawStderr
          }

decodeStrip :: ByteString -> Text
decodeStrip = T.strip . T.toStrict . T.decodeUtf8

-- TODO: Shell escaping
displayCommand :: String -> [String] -> String
displayCommand command args = command <> " " <> unwords args
