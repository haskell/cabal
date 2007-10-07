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

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), PkgInfo(..), Repo(..))
import Network.Hackage.CabalInstall.Fetch (downloadIndex)
import Network.Hackage.CabalInstall.Tar (gunzip)


import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath (dropExtension)
import Text.Printf

-- | 'update' downloads the package list from all known servers
update :: ConfigFlags -> IO ()
update cfg = mapM_ (updateRepo cfg) (configRepos cfg)

updateRepo :: ConfigFlags 
           -> Repo
           -> IO ()
updateRepo cfg repo =
    do printf "Downloading package list from server '%s'\n" (repoURL repo)
       indexPath <- downloadIndex cfg repo
       BS.readFile indexPath >>= BS.writeFile (dropExtension indexPath) . gunzip
