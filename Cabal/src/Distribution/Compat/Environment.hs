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

#ifdef mingw32_HOST_OS
import Foreign.C
import GHC.Windows
#else
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error (throwErrnoIfMinus1_)
import System.Posix.Internals ( withFilePath )
#endif /* mingw32_HOST_OS */

getEnvironment :: IO [(String, String)]
#ifdef mingw32_HOST_OS
-- On Windows, the names of environment variables are case-insensitive, but are
-- often given in mixed-case (e.g. "PATH" is "Path"), so we have to normalise
-- them.
getEnvironment = fmap upcaseVars System.getEnvironment
  where
    upcaseVars = map upcaseVar
    upcaseVar (var, val) = (map toUpper var, val)
#else
getEnvironment = System.getEnvironment
#endif

#ifdef mingw32_HOST_OS

setEnv_ key value = withCWString key $ \k -> withCWString value $ \v -> do
  success <- c_SetEnvironmentVariable k v
  unless success (throwGetLastError "setEnv")
 where
  _ = callStack -- TODO: attach CallStack to exception

# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif /* i386_HOST_ARCH */

foreign import WINDOWS_CCONV unsafe "windows.h SetEnvironmentVariableW"
  c_SetEnvironmentVariable :: LPTSTR -> LPTSTR -> Prelude.IO Bool
#else
setEnv_ key value = do
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum True))
 where
  _ = callStack -- TODO: attach CallStack to exception

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> Prelude.IO CInt
#endif /* mingw32_HOST_OS */
