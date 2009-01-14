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
         ( Repo(..), RemoteRepo(..), LocalRepo(..), AvailablePackageDb(..) )
import Distribution.Client.Fetch
         ( downloadIndex )
import qualified Distribution.Client.Utils as BS
         ( writeFileAtomic )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Client.IndexUtils
         ( getAvailablePackages )
import qualified Paths_cabal_install
         ( version )

import Distribution.Package
         ( PackageName(..), packageId, packageVersion )
import Distribution.Simple.Utils
         ( notice, comparing )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip (decompress)
import System.FilePath (dropExtension)
import Data.List (maximumBy)

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> [Repo] -> IO ()
update verbosity repos = do
  mapM_ (updateRepo verbosity) repos
  checkForSelfUpgrade verbosity repos

updateRepo :: Verbosity -> Repo -> IO ()
updateRepo verbosity repo = case repoKind repo of
  Right LocalRepo -> return ()
  Left remoteRepo -> do
    notice verbosity $ "Downloading package list from server '"
                    ++ show (remoteRepoURI remoteRepo) ++ "'"
    indexPath <- downloadIndex verbosity remoteRepo (repoLocalDir repo)
    BS.writeFileAtomic (dropExtension indexPath) . GZip.decompress
                                               =<< BS.readFile indexPath

checkForSelfUpgrade :: Verbosity -> [Repo] -> IO ()
checkForSelfUpgrade verbosity repos = do
  AvailablePackageDb available _ <- getAvailablePackages verbosity repos

  let self = PackageName "cabal-install"
      pkgs = PackageIndex.lookupPackageName available self
      latestVersion  = packageVersion (maximumBy (comparing packageId) pkgs)
      currentVersion = Paths_cabal_install.version

  if not (null pkgs) && latestVersion > currentVersion
    then notice verbosity $
              "Note: there is a new version of cabal-install available.\n"
           ++ "To upgrade, run: cabal install cabal-install"
    else return ()
