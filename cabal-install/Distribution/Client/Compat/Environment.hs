{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Compat.Environment
-- Copyright   :  (c) Simon Hengel 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A cross-platform library for setting environment variables.
--
-----------------------------------------------------------------------------

module Distribution.Client.Compat.Environment (
  lookupEnv, setEnv
) where

#ifdef mingw32_HOST_OS
import GHC.Windows
import Foreign.Safe
import Foreign.C
import Control.Monad
#else
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error (throwErrnoIfMinus1_)
import System.Posix.Internals ( withFilePath )
#endif /* mingw32_HOST_OS */

#if MIN_VERSION_base(4,6,0)
import System.Environment (lookupEnv)
#else
import System.Environment (getEnv)
import Distribution.Compat.Exception (catchIO)
#endif

#if !MIN_VERSION_base(4,6,0)
-- | @lookupEnv var@ returns the value of the environment variable @var@, or
-- @Nothing@ if there is no such value.
lookupEnv :: String -> IO (Maybe String)
lookupEnv name = (Just `fmap` getEnv name) `catchIO` const (return Nothing)
#endif /* !MIN_VERSION_base(4,6,0) */

-- | @setEnv name value@ sets the specified environment variable to @value@.
--
-- Throws `Control.Exception.IOException` if either @name@ or @value@ is the
-- empty string or contains an equals sign.
setEnv :: String -> String -> IO ()
setEnv key value_
  | null value = error "Distribuiton.Compat.setEnv: empty string"
  | otherwise  = setEnv_ key value
  where
    -- NOTE: Anything that follows NUL is ignored on both POSIX and Windows. We
    -- still strip it manually so that the null check above succeds if a value
    -- starts with NUL.
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()

#ifdef mingw32_HOST_OS

setEnv_ key value = withCWString key $ \k -> withCWString value $ \v -> do
  success <- c_SetEnvironmentVariable k v
  unless success (throwGetLastError "setEnv")

# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif /* i386_HOST_ARCH */

foreign import WINDOWS_CCONV unsafe "windows.h SetEnvironmentVariableW"
  c_SetEnvironmentVariable :: LPTSTR -> LPTSTR -> IO Bool
#else
setEnv_ key value = do
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum True))

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> IO CInt
#endif /* mingw32_HOST_OS */
