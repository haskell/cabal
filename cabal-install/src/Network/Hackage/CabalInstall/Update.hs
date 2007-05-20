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

import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen(..), PkgInfo(..))
import Network.Hackage.CabalInstall.Config (writeKnownPackages)
import Network.Hackage.CabalInstall.TarUtils (extractTarFile, tarballGetFiles)
import Network.Hackage.CabalInstall.Fetch (downloadIndex, packagesDirectory)

import Distribution.Package (PackageIdentifier(..), pkgName, showPackageId)
import Distribution.PackageDescription (PackageDescription(..), readPackageDescription)
import Distribution.Verbosity
import System.FilePath ((</>), joinPath, addExtension, takeExtension)

import Control.Monad (liftM)
import Data.List (isSuffixOf)
import Data.Version (showVersion)

-- | 'update' downloads the package list from all known servers
update :: ConfigFlags -> IO ()
update cfg =
    do packages <- concatMapM servers $ \server ->
           do gettingPkgList output server
              indexPath <- downloadIndex cfg server
              extractTarFile tarPath indexPath
              contents <- tarballGetFiles tarPath indexPath
              let packageDir = packagesDirectory cfg
                  cabalFiles = [ packageDir </> path
                               | path <- contents
                               , ".cabal" == takeExtension path ]
                  v = configVerbose cfg
                  v'= if v == verbose then normal else v
              packageDescriptions <-
                  mapM (readPackageDescription v') cabalFiles
              return $ map (parsePkg server) packageDescriptions
       writeKnownPackages cfg packages
    where servers = configServers cfg
          output = configOutputGen cfg
          tarPath = configTarPath cfg

parsePkg :: String -> PackageDescription -> PkgInfo
parsePkg server description =
    PkgInfo { infoId       = package description
            , infoDeps     = buildDepends description
            , infoSynopsis = synopsis description
            , infoURL      = pkgURL (package description) server
            }

-- | Generate the URL of the tarball for a given package.
pkgURL :: PackageIdentifier -> String -> String
pkgURL pkg base = joinPath [base, pkgName pkg, showVersion (pkgVersion pkg), showPackageId pkg] `addExtension` ".tar.gz"

concatMapM :: (Monad m) => [a] -> (a -> m [b]) -> m [b]
concatMapM amb f = liftM concat (mapM f amb)

