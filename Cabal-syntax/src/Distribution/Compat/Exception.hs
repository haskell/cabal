{-# LANGUAGE CPP #-}

module Distribution.Compat.Exception
  ( catchIO
  , catchExit
  , tryIO
  , displayException
  ) where

import Control.Exception (displayException)
import qualified Control.Exception as Exception
import System.Exit

-- | Try 'IOException'.
tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

-- | Catch 'IOException'.
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

-- | Catch 'ExitCode'
catchExit :: IO a -> (ExitCode -> IO a) -> IO a
catchExit = Exception.catch
