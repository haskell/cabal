-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.Cabal.Clean
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- 
-----------------------------------------------------------------------------
module Network.Hackage.Cabal.Clean
    ( clean
    ) where

import Network.Hackage.Cabal.Types (ConfigFlags(..))

import System.Directory (removeDirectoryRecursive)

-- | 'clean' removes all downloaded packages from the {config-dir}\/packages\/ directory.
clean :: ConfigFlags -> IO ()
clean cfg
    = removeDirectoryRecursive (configCacheDir cfg)

