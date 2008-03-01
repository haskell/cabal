-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Install
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Hackage.List (
  list
  ) where

import Data.List (sortBy, groupBy)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(mconcat,mempty))
import Data.Map as Map (Map)
import qualified Data.Map as Map

import Distribution.Package (PackageIdentifier(..), Package(..))
import Distribution.PackageDescription as Cabal
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Version (Version,showVersion)
import Distribution.Verbosity (Verbosity)

import qualified Hackage.IndexUtils as IndexUtils
import Hackage.Setup (ListFlags(..))
import Hackage.Types (PkgInfo(..), Repo)
import Distribution.Simple.Configure as Cabal (getInstalledPackages)
import Distribution.Simple.Compiler as Cabal (Compiler,PackageDB)
import Distribution.Simple.PackageIndex as Installed
import Distribution.Simple.Program as Cabal (ProgramConfiguration)
import Distribution.Simple.Utils (equating, comparing, lowercase, notice)
import Distribution.Simple.Setup (fromFlag)
import Distribution.InstalledPackageInfo as Installed

-- |Show information about packages
list :: Verbosity
     -> PackageDB
     -> [Repo]
     -> Compiler
     -> ProgramConfiguration
     -> ListFlags
     -> [String]
     -> IO ()
list verbosity packageDB repos comp conf listFlags pats = do
    indexes <- mapM (IndexUtils.readRepoIndex verbosity) repos
    let index = mconcat indexes
        pkgs | null pats = PackageIndex.allPackages index
             | otherwise =
                 concatMap (PackageIndex.searchByNameSubstring index) pats

    instPkgs <- do
        pkgsM <- Cabal.getInstalledPackages verbosity comp packageDB conf
        case pkgsM of
            Nothing -> do
                notice verbosity "Unable to read list of installed packages."
                notice verbosity $ "Your compiler (" ++ show comp ++ ") is not supported."
                notice verbosity "Pretending no packages are installed."
                return mempty
            Just ps ->
                return
                    . Map.fromList $
                    [ (name, maximum versions)
                    | installed <- allPackagesByName ps
                    , let name = pkgName . packageId . Installed.package . head $ installed
                    , let versions =
                            [ pkgVersion . packageId . Installed.package $ i
                            | i <- installed
                            ]
                    ]
    let matches =
          installedFilter instPkgs
          . groupBy (equating (pkgName . packageId))
          . sortBy (comparing nameAndVersion)
          $ pkgs

    if null matches
        then notice verbosity "No mathes found."
        else putStr . unlines . map (showPkgVersions instPkgs) $ matches

   where
    nameAndVersion p = (lowercase name, name, version)
        where name = pkgName (packageId p)
              version = pkgVersion (packageId p)
    installedFilter pkgs
        | fromFlag (listInstalled listFlags) =
            filter (\p -> Map.member (pkgName . packageId . head $ p) pkgs)
        | otherwise = id

showPkgVersions :: Map String Version -> [PkgInfo] -> String
showPkgVersions installedPkgs pkgs = unlines $
    [ " * " ++ name ] ++ map (indent 6) (catMaybes [
      -- does it matter which repo the package came from?
      -- compare to gentoo overlays
      p $ "Latest version available: " ++ showVersion (pkgVersion (packageId pkg))
    , installedVersion >>= return . ("Latest version installed: " ++) . showVersion
    -- TODO: add size of required downloads? need hackage support for this
    , s (Cabal.homepage pd) ("Homepage: " ++)
    , s (Cabal.category pd) ("Category: " ++)
    , p $ "Synopsis: " ++ synopsis (packageDescription (pkgDesc pkg))
    , p $ "License:  " ++ show (Cabal.license pd)
    ])
  where
    p = Just
    s str f
        | not (null str) = Just (f str)
        | otherwise = Nothing
    indent n str = replicate n ' ' ++ str
    pd = packageDescription (pkgDesc pkg)
    name = pkgName (packageId pkg)
    pkg = last pkgs
    installedVersion = Map.lookup name installedPkgs
