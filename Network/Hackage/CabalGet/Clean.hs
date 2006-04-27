-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalGet.Clean
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- 
-----------------------------------------------------------------------------
module Network.Hackage.CabalGet.Clean
    ( clean
    ) where

import Network.Hackage.CabalGet.Types (ConfigFlags)
import Network.Hackage.CabalGet.Fetch (packagesDirectory)

import System.Directory (removeDirectoryRecursive)

-- | 'clean' removes all downloaded packages from the {config-dir}\/packages\/ directory.
clean :: ConfigFlags -> IO ()
clean cfg
    = removeDirectoryRecursive (packagesDirectory cfg)

