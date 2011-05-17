{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}

#if !(defined(__HUGS__) || (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 610))
#define NEW_EXCEPTION
#endif

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
#ifdef NEW_EXCEPTION
onException = Exception.onException
#else
onException io what = io `Exception.catch` \e -> do what
                                                    Exception.throw e
#endif

throwIOIO :: Exception.IOException -> IO a
#ifdef NEW_EXCEPTION
throwIOIO = Exception.throwIO
#else
throwIOIO = Exception.throwIO . Exception.IOException
#endif

tryIO :: IO a -> IO (Either Exception.IOException a)
#ifdef NEW_EXCEPTION
tryIO = Exception.try
#else
tryIO = Exception.tryJust Exception.ioErrors
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#ifdef NEW_EXCEPTION
catchIO = Exception.catch
#else
catchIO = Exception.catchJust Exception.ioErrors
#endif

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
#ifdef NEW_EXCEPTION
catchExit = Exception.catch
#else
catchExit = Exception.catchJust exitExceptions
    where exitExceptions (Exception.ExitException ee) = Just ee
          exitExceptions _                            = Nothing
#endif

