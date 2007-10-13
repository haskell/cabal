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
import Hackage.Config (getKnownPackages)
import Hackage.Types (PkgInfo(..), pkgInfoId, ConfigFlags(..), {- UnresolvedDependency(..)-} )

-- |Show information about packages
list :: ConfigFlags -> [String] -> IO ()
list cfg pats = do
    pkgs <- getKnownPackages cfg
    let pkgs' | null pats = pkgs
              | otherwise = nubBy samePackage (concatMap (findInPkgs pkgs) pats)
    mapM_ doList (groupBy sameName (sortBy (comparing nameAndVersion) pkgs'))
    where
    findInPkgs :: [PkgInfo] -> String -> [PkgInfo]
    findInPkgs pkgs pat = let rx = mkRegexWithOpts pat False False in
        filter (isJust . matchRegex rx . showInfo) pkgs
    showInfo :: PkgInfo -> String
    showInfo pkg = showPackageId (package d) ++ "\n" ++ synopsis d
         where d = packageDescription (pkgDesc pkg)
    nameAndVersion p = (map Char.toLower name, name, version)
        where d = packageDescription (pkgDesc p)
              name = pkgName (package d)
              version = pkgVersion (package d)
    samePackage a b = nameAndVersion a == nameAndVersion b
    sameName a b = pkgName (pkgInfoId a) == pkgName (pkgInfoId b)

doList :: [PkgInfo] -> IO ()
doList ps = do   
    putStr $ padTo 35 $ pkgName (package d) ++ " [" ++ concat (intersperse "," versions) ++ "]"
    putStrLn syn
    where
    info = last ps
    d = packageDescription (pkgDesc info)
    syn = synopsis d
    versions = map (showVersion . pkgVersion . package . packageDescription . pkgDesc) ps
    padTo n s = s ++ (replicate (n - length s) ' ')
