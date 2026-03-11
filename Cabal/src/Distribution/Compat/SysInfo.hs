{-# LANGUAGE CPP #-}

module Distribution.Compat.SysInfo
  ( fullCompilerVersion
  ) where

import Data.Version (Version)
import qualified System.Info as SI

fullCompilerVersion :: Version
#if MIN_VERSION_base(4,15,0)
fullCompilerVersion = SI.fullCompilerVersion
#else
fullCompilerVersion = SI.compilerVersion
#endif
