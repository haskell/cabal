{-# OPTIONS_HADDOCK hide #-}
module Distribution.Compat.Exception (
  SomeException,
  onException,
  catchIO,
  handleIO,
  catchExit,
  throwIOIO
  ) where

import System.Exit
import qualified Control.Exception as Exception
import Control.Exception (SomeException)

onException :: IO a -> IO b -> IO a
onException = Exception.onException

throwIOIO :: Exception.IOException -> IO a
throwIOIO = Exception.throwIO

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

handleIO :: (Exception.IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
catchExit = Exception.catch
