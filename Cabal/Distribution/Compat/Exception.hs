{-# LANGUAGE CPP #-}
module Distribution.Compat.Exception (
  catchIO,
  catchExit,
  tryIO,
  displayException,
  ) where

#ifdef MIN_VERSION_base
#define MINVER_base_48 MIN_VERSION_base(4,8,0)
#else
#define MINVER_base_48 (__GLASGOW_HASKELL__ >= 710)
#endif

import System.Exit
import qualified Control.Exception as Exception

#if MINVER_base_48
import Control.Exception (displayException)
#endif

-- | Try 'IOException'.
tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

-- | Catch 'IOException'.
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

-- | Catch 'ExitCode'
catchExit :: IO a -> (ExitCode -> IO a) -> IO a
catchExit = Exception.catch

#if !MINVER_base_48
displayException :: Exception.Exception e => e -> String
displayException = show
#endif
