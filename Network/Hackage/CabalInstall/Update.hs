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

import Network.Hackage.Client (listPackages)

-- | 'update' downloads the package list from all known servers
update :: ConfigFlags -> IO ()
update cfg
    = do pkgs <- flip concatMapM servers
                 $ \serv -> do gettingPkgList (configOutputGen cfg) serv
                               listPackages serv
         writeKnownPackages cfg pkgs
    where servers = configServers cfg
          concatMapM f = fmap concat . mapM f
