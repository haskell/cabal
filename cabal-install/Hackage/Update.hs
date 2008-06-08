-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Hackage.Update
    ( update
    ) where

import Hackage.Types
import Hackage.Fetch

import Distribution.Simple.Utils (notice)
import Distribution.Verbosity (Verbosity)

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip (decompress)
import System.FilePath (dropExtension)

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> [Repo] -> IO ()
update verbosity = mapM_ (updateRepo verbosity)

updateRepo :: Verbosity -> Repo -> IO ()
updateRepo verbosity repo = do
  notice verbosity $ "Downloading package list from server '"
                  ++ show (repoURI repo) ++ "'"
  indexPath <- downloadIndex verbosity repo
  BS.writeFile (dropExtension indexPath) . GZip.decompress
                                       =<< BS.readFile indexPath
