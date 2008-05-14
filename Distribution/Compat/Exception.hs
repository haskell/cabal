{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-- #hide
module Distribution.Compat.Exception (bracketOnError) where

import Control.Exception as Exception

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 606)
bracketOnError
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracketOnError before after thing =
  Exception.block (do
    a <- before
    Exception.catch
        (Exception.unblock (thing a))
        (\e -> do { after a; Exception.throw e }))
#endif

