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
         ( PackageName(..), packageVersion )
import Distribution.Version
         ( VersionRange(AnyVersion), withinRange )
import Distribution.Simple.Utils
         ( warn, notice )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip (decompress)
import qualified Data.Map as Map
import System.FilePath (dropExtension)
import Data.Maybe      (fromMaybe)
import Control.Monad   (when)

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> [Repo] -> IO ()
update verbosity [] = do
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
update verbosity repos = do
  mapM_ (updateRepo verbosity) repos
  checkForSelfUpgrade verbosity repos

updateRepo :: Verbosity -> Repo -> IO ()
updateRepo verbosity repo = case repoKind repo of
  Right LocalRepo -> return ()
  Left remoteRepo -> do
    notice verbosity $ "Downloading the latest package list from "
                    ++ remoteRepoName remoteRepo
    indexPath <- downloadIndex verbosity remoteRepo (repoLocalDir repo)
    BS.writeFileAtomic (dropExtension indexPath) . GZip.decompress
                                               =<< BS.readFile indexPath

checkForSelfUpgrade :: Verbosity -> [Repo] -> IO ()
checkForSelfUpgrade verbosity repos = do
  AvailablePackageDb available prefs <- getAvailablePackages verbosity repos

  let self = PackageName "cabal-install"
      preferredVersionRange  = fromMaybe AnyVersion (Map.lookup self prefs)
      currentVersion         = Paths_cabal_install.version
      laterPreferredVersions =
        [ packageVersion pkg
        | pkg <- PackageIndex.lookupPackageName available self
        , let version = packageVersion pkg
        , version > currentVersion
        , version `withinRange` preferredVersionRange ]

  when (not (null laterPreferredVersions)) $
    notice verbosity $
         "Note: there is a new version of cabal-install available.\n"
      ++ "To upgrade, run: cabal install cabal-install"

