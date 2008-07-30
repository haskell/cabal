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
    (onException, catchIO, catchExit)
    where

import System.Exit
import Control.Exception as Exception

#ifndef NEW_EXCEPTION
onException :: IO a -> IO () -> IO a
onException io what = io `Exception.catch` \e -> do what
                                                    Exception.throw e
#endif

catchIO :: IO a -> (IOException -> IO a) -> IO a
#ifdef NEW_EXCEPTION
catchIO = Exception.catch
#else
catchIO io handler = io `Exception.catch` handler'
    where handler' (IOException ioe) = handler ioe
          handler' e                 = throw e
#endif

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
#ifdef NEW_EXCEPTION
catchExit = Exception.catch
#else
catchExit io handler = io `Exception.catch` handler'
    where handler' (ExitException ee) = handler ee
          handler' e                  = throw e
#endif

