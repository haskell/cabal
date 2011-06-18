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
import Data.List (find, nub)
import Distribution.Package
import Distribution.PackageDescription
import Network.Hackage.CabalInstall.Config (getKnownPackages)
import Network.Hackage.CabalInstall.Types (PkgInfo(..), ConfigFlags(..), UnresolvedDependency(..)
                                      ,OutputGen(..))

-- |Show information about packages
list :: ConfigFlags -> [String] -> IO ()
list cfg pats = do
    pkgs <- getKnownPackages cfg
    mapM_ doList $ if null pats then pkgs else nub (concatMap (findInPkgs pkgs) pats)
    where
    findInPkgs :: [PkgInfo] -> String -> [PkgInfo]
    findInPkgs pkgs pat = let rx = mkRegexWithOpts pat False False in
        filter (isJust . matchRegex rx . showInfo) pkgs
    showInfo :: PkgInfo -> String
    showInfo pkg = showPackageId (infoId pkg) ++ "\n" ++ infoSynopsis pkg

doList :: PkgInfo -> IO ()
doList info = do
    putStr . (if null syn then id else padTo 25) . showPackageId . infoId $ info
    putStrLn syn
    where
    syn = infoSynopsis info
    padTo n s = s ++ (replicate (n - length s) ' ')
