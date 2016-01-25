{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

module Distribution.Compat.Environment (getEnvironment)
       where

import qualified System.Environment as System

#ifdef mingw32_HOST_OS
import qualified Data.Char as Char (toUpper)
#endif

getEnvironment :: IO [(String, String)]
#ifdef mingw32_HOST_OS
-- On Windows, the names of environment variables are case-insensitive, but are
-- often given in mixed-case (e.g. "PATH" is "Path"), so we have to normalise
-- them.
getEnvironment = fmap upcaseVars System.getEnvironment
  where
    upcaseVars = map upcaseVar
    upcaseVar (var, val) = (map Char.toUpper var, val)
#else
getEnvironment = System.getEnvironment
#endif
