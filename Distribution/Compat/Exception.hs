{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-- #hide
module Distribution.Compat.Exception (
  SomeException,
  onException,
  catchIO,
  catchExit,
  throwIOIO
  ) where

import System.Exit
import qualified Control.Exception as Exception
#if MIN_VERSION_base(4,0,0)
import Control.Exception (SomeException)
#else
import Control.Exception (Exception)
type SomeException = Exception
#endif

onException :: IO a -> IO b -> IO a
#if MIN_VERSION_base(4,0,0)
onException = Exception.onException
#else
onException io what = io `Exception.catch` \e -> do what
                                                    Exception.throw e
#endif

throwIOIO :: Exception.IOException -> IO a
#if MIN_VERSION_base(4,0,0)
throwIOIO = Exception.throwIO
#else
throwIOIO = Exception.throwIO . Exception.IOException
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#if MIN_VERSION_base(4,0,0)
catchIO = Exception.catch
#else
catchIO = Exception.catchJust Exception.ioErrors
#endif

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
#if MIN_VERSION_base(4,0,0)
catchExit = Exception.catch
#else
catchExit = Exception.catchJust exitExceptions
    where exitExceptions (Exception.ExitException ee) = Just ee
          exitExceptions _                            = Nothing
#endif
