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
import Data.Maybe (catMaybes, isJust)
import Data.List (find, nubBy, sortBy)
import Data.Char as Char (toLower)
import Data.Ord  (comparing)
import Distribution.Package
import Distribution.PackageDescription
import Network.Hackage.CabalInstall.Config (getKnownPackages)
import Network.Hackage.CabalInstall.Dependency (finalizePackage, listInstalledPackages)
import Network.Hackage.CabalInstall.Types (PkgInfo(..), ConfigFlags(..), UnresolvedDependency(..)
                                      ,OutputGen(..))

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
    showInfo pkg = showPackageId (package (packageDescription pkg)) 
                   ++ "\n" ++ synopsis (packageDescription pkg)
    nameAndVersion p = (map Char.toLower name, name, version)
        where name = pkgName (package (packageDescription p))
              version = pkgVersion (package (packageDescription p))
    samePackage a b = nameAndVersion a == nameAndVersion b

doList :: PkgInfo -> IO ()
doList info = do
    putStr . (if null syn then id else padTo 25) . showPackageId . package . packageDescription $ info
    putStrLn syn
    where
    syn = synopsis $ packageDescription info
    padTo n s = s ++ (replicate (n - length s) ' ')
