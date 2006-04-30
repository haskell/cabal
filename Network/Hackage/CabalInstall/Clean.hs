-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Clean
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- 
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Clean
    ( clean
    ) where

import Network.Hackage.CabalInstall.Types (ConfigFlags)
import Network.Hackage.CabalInstall.Fetch (packagesDirectory)

import System.Directory (removeDirectoryRecursive)

-- | 'clean' removes all downloaded packages from the {config-dir}\/packages\/ directory.
clean :: ConfigFlags -> IO ()
clean cfg
    = removeDirectoryRecursive (packagesDirectory cfg)

