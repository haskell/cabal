{-# LANGUAGE CPP #-}
module Distribution.Compat.Exception (
  catchIO,
  catchExit,
  tryIO,
  displayException,
  ) where

import System.Exit
import qualified Control.Exception as Exception
import Control.Exception (displayException)

-- | Try 'IOException'.
tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

-- | Catch 'IOException'.
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

-- | Catch 'ExitCode'
catchExit :: IO a -> (ExitCode -> IO a) -> IO a
catchExit = Exception.catch
