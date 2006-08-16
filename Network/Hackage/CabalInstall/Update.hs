-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Update
    ( update
    ) where

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen(..))
import Network.Hackage.CabalInstall.Config (writeKnownPackages)
import Network.Hackage.CabalInstall.TarUtils (extractTarFile)
import Network.Hackage.CabalInstall.Fetch (downloadIndex)
import Network.Hackage.Client (listPackages)

-- | 'update' downloads the package list from all known servers
update :: ConfigFlags -> IO ()
update cfg
    = if length servers > 1
        then fail "Error: only one server at time is supported."
        else do let serv = head servers
                gettingPkgList (configOutputGen cfg) serv
                indexFilePath <- downloadIndex cfg serv
                extractTarFile "/usr/bin/tar" indexFilePath
                pkgs <- listPackages cfg serv
                writeKnownPackages cfg pkgs
    where servers = configServers cfg
