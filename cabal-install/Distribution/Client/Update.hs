-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Update
    ( update
    ) where

import Distribution.Client.Types
import Distribution.Client.Fetch

import Distribution.Simple.Utils (notice)
import Distribution.Verbosity (Verbosity)

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip (decompress)
import System.FilePath (dropExtension)

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> [Repo] -> IO ()
update verbosity = mapM_ (updateRepo verbosity)

updateRepo :: Verbosity -> Repo -> IO ()
updateRepo verbosity repo = case repoKind repo of
  Right LocalRepo -> return ()
  Left remoteRepo -> do
    notice verbosity $ "Downloading package list from server '"
                    ++ show (remoteRepoURI remoteRepo) ++ "'"
    indexPath <- downloadIndex verbosity remoteRepo (repoLocalDir repo)
    BS.writeFile (dropExtension indexPath) . GZip.decompress
                                         =<< BS.readFile indexPath
