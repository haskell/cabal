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
import Distribution.Compat.FilePath (joinPaths)

import Control.Monad (liftM)
import Data.List (isSuffixOf)

-- | 'update' downloads the package list from all known servers
update :: ConfigFlags -> IO ()
update cfg =
    do packages <- concatMapM servers $ \server ->
           do gettingPkgList output server
              indexPath <- downloadIndex cfg server
              extractTarFile "/usr/bin/tar" indexPath
              contents <- tarballGetFiles "/usr/bin/tar" indexPath
              let packageDir = packagesDirectory cfg
                  cabalFiles = [ packageDir `joinPaths` path
                               | path <- contents
                               , ".cabal" `isSuffixOf` path ]
              packageDescriptions <- mapM readPackageDescription cabalFiles
              return $ map (parsePkg server) packageDescriptions
       writeKnownPackages cfg packages
    where servers = configServers cfg
          output = configOutputGen cfg

parsePkg :: String -> PackageDescription -> PkgInfo
parsePkg server description =
    PkgInfo { infoId       = package description
            , infoDeps     = buildDepends description
            , infoSynopsis = synopsis description
            , infoURL      = pkgURL (package description) server
            }

pkgURL :: PackageIdentifier -> String -> String
pkgURL identifier base = concat [base, "/", pkgName identifier, "/", showPackageId identifier, ".tar.gz"]

concatMapM :: (Monad m) => [a] -> (a -> m [b]) -> m [b]
concatMapM amb f = liftM concat (mapM f amb)

