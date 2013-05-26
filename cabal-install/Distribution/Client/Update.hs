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
         ( Repo(..), RemoteRepo(..), LocalRepo(..), SourcePackageDb(..) )
import Distribution.Client.HttpUtils
         ( DownloadResult(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Client.IndexUtils
         ( getSourcePackages, updateRepoIndexCache )
import qualified Paths_cabal_install
         ( version )

import Distribution.Package
         ( PackageName(..), packageVersion )
import Distribution.Version
         ( anyVersion, withinRange )
import Distribution.Simple.Utils
         ( writeFileAtomic, warn, notice )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.ByteString.Lazy       as BS
import Distribution.Client.GZipUtils (maybeDecompress)
import qualified Data.Map as Map
import System.FilePath (dropExtension)
import Data.Maybe      (fromMaybe)
import Control.Monad   (unless)

-- | 'update' downloads the package list from all known servers
update :: Verbosity -> [Repo] -> IO ()
update verbosity [] =
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
    downloadResult <- downloadIndex verbosity remoteRepo (repoLocalDir repo)
    case downloadResult of
      FileAlreadyInCache -> return ()
      FileDownloaded indexPath -> do
        writeFileAtomic (dropExtension indexPath) . maybeDecompress
                                                =<< BS.readFile indexPath
        updateRepoIndexCache verbosity repo

checkForSelfUpgrade :: Verbosity -> [Repo] -> IO ()
checkForSelfUpgrade verbosity repos = do
  SourcePackageDb sourcePkgIndex prefs <- getSourcePackages verbosity repos

  let self = PackageName "cabal-install"
      preferredVersionRange  = fromMaybe anyVersion (Map.lookup self prefs)
      currentVersion         = Paths_cabal_install.version
      laterPreferredVersions =
        [ packageVersion pkg
        | pkg <- PackageIndex.lookupPackageName sourcePkgIndex self
        , let version = packageVersion pkg
        , version > currentVersion
        , version `withinRange` preferredVersionRange ]

  unless (null laterPreferredVersions) $
    notice verbosity $
         "Note: there is a new version of cabal-install available.\n"
      ++ "To upgrade, run: cabal install cabal-install"
