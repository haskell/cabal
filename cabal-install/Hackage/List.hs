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

import Data.List (nub, sortBy, groupBy)
import Data.Monoid (Monoid(mconcat))

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version (showVersion)
import Distribution.Verbosity (Verbosity)

import qualified Hackage.RepoIndex as RepoIndex
import Hackage.Types (PkgInfo(..), Repo)
import Hackage.Utils (equating, comparing, intercalate, lowercase)

-- |Show information about packages
list :: Verbosity -> [Repo] -> [String] -> IO ()
list verbosity repos pats = do
    indexes <- mapM (RepoIndex.read verbosity) repos
    let index = mconcat indexes
        pkgs | null pats = RepoIndex.allPackages index
             | otherwise =
                 concatMap (RepoIndex.lookupPackageNameSubstring index) pats
    putStrLn
      . unlines
      . map showPkgVersions
      . groupBy (equating (pkgName . pkgInfoId))
      . sortBy (comparing nameAndVersion)
      $ pkgs

  where
    nameAndVersion p = (lowercase name, name, version)
        where name = pkgName (pkgInfoId p)
              version = pkgVersion (pkgInfoId p)


showPkgVersions :: [PkgInfo] -> String
showPkgVersions pkgs =
    padTo 35 $ pkgName (pkgInfoId pkg)
            ++ " ["
            ++ intercalate ", " (map showVersion versions)
            ++ "] "
    ++ synopsis (packageDescription (pkgDesc pkg))
  where
    pkg = last pkgs
    versions = nub (map (pkgVersion . pkgInfoId) pkgs)
    padTo n s = s ++ (replicate (n - length s) ' ')
