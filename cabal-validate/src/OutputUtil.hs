-- | Utilities for printing terminal output.
module OutputUtil
  ( printHeader
  , withTiming
  ) where

import Control.Exception (catch)
import qualified System.Console.Terminal.Size as Terminal
import System.Process.Typed (ExitCodeException)

import ANSI (SGR (BoldCyan, BoldGreen, BoldRed, Reset), setSGR)
import ClockUtil (AbsoluteTime, diffAbsoluteTime, formatDiffTime, getAbsoluteTime)
import System.Exit (exitFailure)

-- | Get the width of the current terminal, or 80 if no width can be determined.
getTerminalWidth :: IO Int
getTerminalWidth = maybe 80 Terminal.width <$> Terminal.size @Int

-- | Print a header for a given step.
--
-- This is colorful and hard to miss in the output.
printHeader
  :: String
  -- ^ Title to print.
  -> IO ()
printHeader title = do
  columns <- getTerminalWidth
  let left = 3
      right = columns - length title - left - 2
      header =
        setSGR [BoldCyan]
          <> replicate left '═'
          <> " "
          <> title
          <> " "
          <> replicate right '═'
          <> setSGR [Reset]
  putStrLn header

-- | Run an `IO` action and print duration information after it finishes.
withTiming
  :: AbsoluteTime
  -- ^ Start time for the whole @cabal-validate@ run.
  -> String
  -- ^ Name for describing the action.
  --
  -- Used in a sentence like "@title@ finished after 16.34s".
  -> IO a
  -- ^ Action to time.
  -> IO a
withTiming startTime title action = do
  startTime' <- getAbsoluteTime

  result <-
    (Right <$> action)
      `catch` (\exception -> pure (Left (exception :: ExitCodeException)))

  endTime <- getAbsoluteTime

  let duration = diffAbsoluteTime endTime startTime'
      totalDuration = diffAbsoluteTime endTime startTime

  case result of
    Right inner -> do
      putStrLn $
        setSGR [BoldGreen]
          <> title
          <> " finished after "
          <> formatDiffTime duration
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGR [Reset]

      pure inner
    Left _procFailed -> do
      putStrLn $
        setSGR [BoldRed]
          <> title
          <> " failed after "
          <> formatDiffTime duration
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGR [Reset]

      -- TODO: `--keep-going` mode.
      exitFailure
