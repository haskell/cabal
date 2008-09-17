{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}

#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ >= 609)
#define NEW_EXCEPTION
#endif

module Distribution.Compat.Exception
    (onException, catchIO, catchExit, throwIOIO)
    where

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
throwIOIO ioe = Exception.throwIO (Exception.IOException ioe)
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#ifdef NEW_EXCEPTION
catchIO = Exception.catch
#else
catchIO io handler = io `Exception.catch` handler'
    where handler' (Exception.IOException ioe) = handler ioe
          handler' e                           = Exception.throw e
#endif

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
#ifdef NEW_EXCEPTION
catchExit = Exception.catch
#else
catchExit io handler = io `Exception.catch` handler'
    where handler' (Exception.ExitException ee) = handler ee
          handler' e                            = Exception.throw e
#endif

