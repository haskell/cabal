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
module Hackage.List
    ( list    -- :: ConfigFlags -> [UnresolvedDependency] -> IO ()
    ) where

import Text.Regex
import Data.Maybe (isJust)
import Data.List (nubBy, sortBy, groupBy, intersperse)
import Data.Char as Char (toLower)
import Data.Ord  (comparing)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version (showVersion)
import Hackage.Index (getKnownPackages)
import Hackage.Types (PkgInfo(..), ConfigFlags(..), {- UnresolvedDependency(..)-} )

-- |Show information about packages
list :: ConfigFlags -> [String] -> IO ()
list cfg pats = do
    pkgs <- getKnownPackages cfg
    let pkgs' | null pats = pkgs
              | otherwise = nubBy samePackage (concatMap (findInPkgs pkgs) pats')
        pats' = map lcase pats
    putStrLn
      . unlines
      . map (showPkgVersions . map (packageDescription . pkgDesc))
      . groupBy sameName
      . sortBy (comparing nameAndVersion)
      $ pkgs'

  where
    findInPkgs :: [PkgInfo] -> String -> [PkgInfo]
    findInPkgs pkgs pat =
        filter (isInfixOf pat . lcase . pkgName . pkgInfoId) pkgs
    lcase = map Char.toLower
    nameAndVersion p = (lcase name, name, version)
        where name = pkgName (pkgInfoId p)
              version = pkgVersion (pkgInfoId p)
    samePackage a b = nameAndVersion a == nameAndVersion b
    sameName a b = pkgName (pkgInfoId a) == pkgName (pkgInfoId b)

showPkgVersions :: [PackageDescription] -> String
showPkgVersions pkgs =
    padTo 35 (pkgName (package pkg)
          ++ " [" ++ concat (intersperse ", " versions) ++ "] ")
    ++ synopsis pkg
  where
    pkg = last pkgs
    versions = map (showVersion . pkgVersion . package) pkgs
    padTo n s = s ++ (replicate (n - length s) ' ')
