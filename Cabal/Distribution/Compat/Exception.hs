module Distribution.Compat.Exception (
     catchIO,
     catchExit,
     throwIOIO,
     tryIO,
  ) where

import System.Exit
import qualified Control.Exception as Exception

throwIOIO :: Exception.IOException -> IO a
throwIOIO = Exception.throwIO

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
catchExit = Exception.catch
