module OutputUtil
  ( printHeader
  , withTiming
  ) where

import Control.Exception (catch)
import System.Console.ANSI
  ( Color (Cyan, Green, Red)
  , ColorIntensity (Vivid)
  , ConsoleIntensity (BoldIntensity)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor, SetConsoleIntensity)
  , setSGRCode
  )
import qualified System.Console.Terminal.Size as Terminal
import System.Process.Typed (ExitCodeException)

import Cli (Opts (..))
import ClockUtil (diffAbsoluteTime, formatDiffTime, getAbsoluteTime)
import System.Exit (exitFailure)

getTerminalWidth :: IO Int
getTerminalWidth = maybe 80 Terminal.width <$> Terminal.size @Int

printHeader :: String -> IO ()
printHeader title = do
  columns <- getTerminalWidth
  let left = 3
      right = columns - length title - left - 2
      header =
        setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]
          <> replicate left '═'
          <> " "
          <> title
          <> " "
          <> replicate right '═'
          <> setSGRCode [Reset]
  putStrLn header

withTiming :: Opts -> String -> IO a -> IO a
withTiming opts title action = do
  startTime' <- getAbsoluteTime

  result <-
    (Right <$> action)
      `catch` (\exception -> pure (Left (exception :: ExitCodeException)))

  endTime <- getAbsoluteTime

  let duration = diffAbsoluteTime endTime startTime'
      totalDuration = diffAbsoluteTime endTime (startTime opts)

  case result of
    Right inner -> do
      putStrLn $
        setSGRCode [SetColor Foreground Vivid Green]
          <> title
          <> " finished after "
          <> formatDiffTime duration
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGRCode [Reset]

      pure inner
    Left _procFailed -> do
      putStrLn $
        setSGRCode [SetColor Foreground Vivid Red]
          <> title
          <> " failed after "
          <> formatDiffTime duration
          <> "\nTotal time so far: "
          <> formatDiffTime totalDuration
          <> setSGRCode [Reset]

      -- TODO: `--keep-going` mode.
      exitFailure
