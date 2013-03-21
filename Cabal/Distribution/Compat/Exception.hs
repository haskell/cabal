module Distribution.Compat.Exception (
     Exception.IOException,
     onException,
     catchIO,
     catchExit,
     throwIOIO,
     tryIO,
  ) where

import System.Exit
import qualified Control.Exception as Exception

onException :: IO a -> IO b -> IO a
onException = Exception.onException

throwIOIO :: Exception.IOException -> IO a
throwIOIO = Exception.throwIO

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
catchExit = Exception.catch
