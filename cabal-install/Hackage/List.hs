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

import Data.List (sortBy, groupBy, sort, nub, intersperse)
import Data.Maybe (listToMaybe, fromJust)
import Data.Monoid (Monoid(mconcat))
import Control.Monad (MonadPlus(mplus))
import Control.Exception (assert)

import Text.PrettyPrint.HughesPJ
import Distribution.Text
         ( Text(disp), display )

import Distribution.Package (PackageIdentifier(..), Package(..))
import Distribution.License (License)
import qualified Distribution.PackageDescription as Available
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Version (Version)
import Distribution.Verbosity (Verbosity)

import qualified Hackage.IndexUtils as IndexUtils (readRepoIndex)
import Hackage.Setup (ListFlags(..))
import Hackage.Types (AvailablePackage(..), Repo)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Compiler (Compiler,PackageDB)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Utils (equating, comparing, notice)
import Distribution.Simple.Setup (fromFlag)

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
    Just installed <- getInstalledPackages verbosity comp packageDB conf
    available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
    let pkgs | null pats = (PackageIndex.allPackages installed
                           ,PackageIndex.allPackages available)
             | otherwise =
                 (concatMap (PackageIndex.searchByNameSubstring installed) pats
                 ,concatMap (PackageIndex.searchByNameSubstring available) pats)
        matches = installedFilter
                . map (uncurry mergePackageInfo)
                $ uncurry mergePackages pkgs

    if simpleOutput
      then putStr $ unlines
             [ name pkg ++ " " ++ display version
             | pkg <- matches
             , version <- if onlyInstalled
                            then              installedVersions pkg
                            else nub . sort $ installedVersions pkg
                                           ++ availableVersions pkg ]
      else
        if null matches
            then notice verbosity "No matches found."
            else putStr $ unlines (map showPackageInfo matches)
  where
    installedFilter
      | onlyInstalled = filter (not . null . installedVersions)
      | otherwise     = id
    onlyInstalled = fromFlag (listInstalled listFlags)
    simpleOutput  = fromFlag (listSimpleOutput listFlags)

-- | The info that we can display for each package. It is information per
-- package name and covers all installed and avilable versions.
--
data PackageDisplayInfo = PackageDisplayInfo {
    name              :: String,
    installedVersions :: [Version],
    availableVersions :: [Version],
    homepage          :: String,
    category          :: String,
    synopsis          :: String,
    license           :: License
  }

showPackageInfo :: PackageDisplayInfo -> String
showPackageInfo pkg =
  renderStyle (style {lineLength = 80, ribbonsPerLine = 1}) $
     text " *" <+> text (name pkg)
     $+$
     (nest 6 $ vcat [
       maybeShow (availableVersions pkg)
         "Latest version available:"
         (disp . maximum)
     , maybeShow (installedVersions pkg)
         "Latest version installed:"
         (disp . maximum)
     , maybeShow (homepage pkg) "Homepage:" text
     , maybeShow (category pkg) "Category:" text
     , maybeShow (synopsis pkg) "Synopsis:" reflowParas
     , text "License: " <+> text (show (license pkg))
     ])
     $+$ text ""
  where
    maybeShow [] _ _ = empty
    maybeShow l  s f = text s <+> (f l)
    reflowParas = vcat
                . intersperse (text "")           -- re-insert blank lines
                . map (fsep . map text . concatMap words)  -- reflow paras
                . filter (/= [""])
                . groupBy (\x y -> "" `notElem` [x,y])  -- break on blank lines
                . lines

-- | We get the 'PackageDisplayInfo' by combining the info for the installed
-- and available versions of a package.
--
-- * We're building info about a various versions of a single named package so
-- the input package info records are all supposed to refer to the same
-- package name.
--
mergePackageInfo :: [InstalledPackageInfo]
                 -> [AvailablePackage]
                 -> PackageDisplayInfo
mergePackageInfo installed available =
  assert (length installed + length available > 0) $
  PackageDisplayInfo {
    name              = combine (pkgName . packageId) latestAvailable
                                (pkgName . packageId) latestInstalled,
    installedVersions = map (pkgVersion . packageId) installed,
    availableVersions = map (pkgVersion . packageId) available,
    homepage          = combine Available.homepage latestAvailableDesc
                                Installed.homepage latestInstalled,
    category          = combine Available.category latestAvailableDesc
                                Installed.category latestInstalled,
    synopsis          = combine Available.synopsis latestAvailableDesc
                                Installed.description latestInstalled,
    license           = combine Available.license  latestAvailableDesc
                                Installed.license latestInstalled
  }
  where
    combine f x g y = fromJust (fmap f x `mplus` fmap g y)
    latestInstalled = latestOf installed
    latestAvailable = latestOf available
    latestAvailableDesc = fmap (Available.packageDescription . packageDescription)
                          latestAvailable
    latestOf :: Package pkg => [pkg] -> Maybe pkg
    latestOf = listToMaybe . sortBy (comparing (pkgVersion . packageId))

-- | Rearrange installed and available packages into groups referring to the
-- same package by name. In the result pairs, the lists are guaranteed to not
-- both be empty.
--
mergePackages ::   [InstalledPackageInfo] -> [AvailablePackage]
              -> [([InstalledPackageInfo],   [AvailablePackage])]
mergePackages installed available =
    map (\(is, as) -> (maybe [] snd is
                    ,maybe [] snd as))
  $ mergeBy (\i a -> fst i `compare` fst a)
            (groupOn (pkgName . packageId) installed)
            (groupOn (pkgName . packageId) available)

groupOn :: Ord key => (a -> key) -> [a] -> [(key,[a])]
groupOn key = map (\xs -> (key (head xs), xs))
            . groupBy (equating key)
            . sortBy (comparing key)

-- | Generic merging utility. For sorted input lists this is a full outer join.
--
-- * The result list never contains @(Nothing, Nothing)@.
--
mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> [(Maybe a, Maybe b)]
mergeBy cmp = merge
  where
    merge []     ys     = [ (Nothing, Just y) | y <- ys]
    merge xs     []     = [ (Just x, Nothing) | x <- xs]
    merge (x:xs) (y:ys) =
      case x `cmp` y of
        GT -> (Nothing, Just y) : merge (x:xs) ys
        EQ -> (Just x,  Just y) : merge xs     ys
        LT -> (Just x, Nothing) : merge xs  (y:ys)
