{- FOURMOLU_DISABLE -}
{-# LANGUAGE CPP #-}

module IntegrationTests2.CPP
  ( removePathForcibly
  , isMingw32
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

#if MIN_VERSION_directory(1,2,7)
import System.Directory (removePathForcibly)
#endif

#if !MIN_VERSION_directory(1,2,7)
removePathForcibly :: FilePath -> IO ()
removePathForcibly = removeDirectoryRecursive
#endif

isMingw32 :: Bool
#ifndef mingw32_HOST_OS
isMingw32 = True
#else
isMingw32 = False
#endif
