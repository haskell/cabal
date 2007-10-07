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

import System.Directory (removeDirectoryRecursive)

-- | 'clean' removes all downloaded packages from the {config-dir}\/packages\/ directory.
clean :: ConfigFlags -> IO ()
clean cfg
    = removeDirectoryRecursive (configCacheDir cfg)

