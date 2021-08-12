{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Environment
       ( getEnvironment, lookupEnv, setEnv, unsetEnv )
       where

import Prelude ()
import qualified Prelude
import Distribution.Compat.Prelude

import qualified System.Environment as System
import System.Environment ( lookupEnv, unsetEnv )

import Distribution.Compat.Stack

import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error (throwErrnoIfMinus1_)
import System.Posix.Internals ( withFilePath )


getEnvironment :: IO [(String, String)]
getEnvironment = System.getEnvironment


-- | @setEnv name value@ sets the specified environment variable to @value@.
--
-- Throws `Control.Exception.IOException` if either @name@ or @value@ is the
-- empty string or contains an equals sign.
setEnv :: String -> String -> IO ()
setEnv key value_ = setEnv_ key value
  where
    -- NOTE: Anything that follows NUL is ignored on both POSIX and Windows. We
    -- still strip it manually so that the null check above succeeds if a value
    -- starts with NUL.
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()
setEnv_ key value = do
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum True))
 where
  _ = callStack -- TODO: attach CallStack to exception

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> Prelude.IO CInt
