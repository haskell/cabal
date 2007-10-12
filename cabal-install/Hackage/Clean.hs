-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Clean
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- 
-----------------------------------------------------------------------------
module Hackage.Clean
    ( clean
    ) where

import Hackage.Types (ConfigFlags(..))
import Hackage.Utils (fileNotFoundExceptions)

import System.Directory (removeDirectoryRecursive)
import Control.Exception (catchJust)

-- | 'clean' removes all downloaded packages from the {config-dir}\/packages\/ directory.
clean :: ConfigFlags -> IO ()
clean cfg
    = catchJust fileNotFoundExceptions
        (removeDirectoryRecursive (configCacheDir cfg))
	-- The packages dir may not exist if it's already cleaned:
	(const (return ()))
