-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Install
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.List
    ( list    -- :: ConfigFlags -> [UnresolvedDependency] -> IO ()
    ) where

import Text.Regex
import Data.Maybe (isJust)
import Data.List (nubBy, sortBy)
import Data.Char as Char (toLower)
import Data.Ord  (comparing)
import Distribution.Package
import Distribution.PackageDescription
import Network.Hackage.CabalInstall.Config (getKnownPackages)
import Network.Hackage.CabalInstall.Types (PkgInfo(..), ConfigFlags(..), UnresolvedDependency(..))

-- |Show information about packages
list :: ConfigFlags -> [String] -> IO ()
list cfg pats = do
    pkgs <- getKnownPackages cfg
    let pkgs' | null pats = pkgs
              | otherwise = nubBy samePackage (concatMap (findInPkgs pkgs) pats)
    mapM_ doList (sortBy (comparing nameAndVersion) pkgs')
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

doList :: PkgInfo -> IO ()
doList info = do   
    putStr . (if null syn then id else padTo 25) . showPackageId . package $ d
    putStrLn syn
    where
    d = packageDescription (pkgDesc info)
    syn = synopsis d
    padTo n s = s ++ (replicate (n - length s) ' ')
