-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Install
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Distribution.Client.List (
  list, info
  ) where

import Data.List (sortBy, groupBy, sort, nub, intersperse, maximumBy)
import Data.Maybe (listToMaybe, fromJust, fromMaybe)
import Control.Monad (MonadPlus(mplus), join)
import Control.Exception (assert)

import Text.PrettyPrint.HughesPJ as Disp
import Distribution.Text
         ( Text(disp), display )

import Distribution.Package
         ( PackageName(..), Package(..), packageName, packageVersion
         , Dependency(..), thisPackageVersion )
import Distribution.ModuleName (ModuleName)
import Distribution.License (License)
import qualified Distribution.PackageDescription as Available
import Distribution.PackageDescription
         ( Flag(..), FlagName(..) )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Version (Version)
import Distribution.Verbosity (Verbosity)

import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages, disambiguateDependencies )
import Distribution.Client.Setup (ListFlags(..), InfoFlags(..))
import Distribution.Client.Types
         ( AvailablePackage(..), Repo, AvailablePackageDb(..)
         , UnresolvedDependency(..) )
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Compiler (Compiler,PackageDB)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Utils (equating, comparing, notice)
import Distribution.Simple.Setup (fromFlag)

import Distribution.Client.Utils (mergeBy, MergeResult(..))

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
    AvailablePackageDb available _ <- getAvailablePackages verbosity repos
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
             [ display (pkgname pkg) ++ " " ++ display version
             | pkg <- matches
             , version <- if onlyInstalled
                            then              installedVersions pkg
                            else nub . sort $ installedVersions pkg
                                           ++ availableVersions pkg ]
      else
        if null matches
            then notice verbosity "No matches found."
            else putStr $ unlines (map showPackageSummaryInfo matches)
  where
    installedFilter
      | onlyInstalled = filter (not . null . installedVersions)
      | otherwise     = id
    onlyInstalled = fromFlag (listInstalled listFlags)
    simpleOutput  = fromFlag (listSimpleOutput listFlags)

info :: Verbosity
     -> PackageDB
     -> [Repo]
     -> Compiler
     -> ProgramConfiguration
     -> InfoFlags
     -> [UnresolvedDependency] --FIXME: just package names? or actually use the constraint
     -> IO ()
info verbosity packageDB repos comp conf _listFlags deps = do
  AvailablePackageDb available _ <- getAvailablePackages verbosity repos
  deps' <- IndexUtils.disambiguateDependencies available deps
  Just installed <- getInstalledPackages verbosity comp packageDB conf
  let deps'' = [ name | UnresolvedDependency (Dependency name _) _ <- deps' ]
  let pkgs = (concatMap (PackageIndex.lookupPackageName installed) deps''
             ,concatMap (PackageIndex.lookupPackageName available) deps'')
      pkgsinfo = map (uncurry mergePackageInfo)
               $ uncurry mergePackages pkgs
  
  pkgsinfo' <- mapM updateFileSystemPackageDetails pkgsinfo
  putStr $ unlines (map showPackageDetailedInfo pkgsinfo')

-- | The info that we can display for each package. It is information per
-- package name and covers all installed and avilable versions.
--
data PackageDisplayInfo = PackageDisplayInfo {
    pkgname           :: PackageName,
    installedVersions :: [Version],
    availableVersions :: [Version],
    homepage          :: String,
    bugReports        :: String,
    sourceRepo        :: String,
    synopsis          :: String,
    description       :: String,
    category          :: String,
    license           :: License,
--    copyright         :: String, --TODO: is this useful?
    author            :: String,
    maintainer        :: String,
    dependencies      :: [Dependency],
    flags             :: [Flag],
    executables       :: [String],
    modules           :: [ModuleName],
    haddockHtml       :: FilePath,
    haveTarball       :: Bool
  }

showPackageSummaryInfo :: PackageDisplayInfo -> String
showPackageSummaryInfo pkg =
  renderStyle (style {lineLength = 80, ribbonsPerLine = 1}) $
     char '*' <+> disp (pkgname pkg)
     $+$
     (nest 4 $ vcat [
       text "Latest version available:" <+>
       case availableVersions pkg of
         [] -> text "[ Not available from server ]"
         vs -> disp (maximum vs)
     , text "Latest version installed:" <+>
       case installedVersions pkg of
         [] -> text "[ Not installed ]"
         vs -> disp (maximum vs)
     , maybeShow (homepage pkg) "Homepage:" text
     , maybeShow (synopsis pkg) "Synopsis:" reflowParagraphs
     , text "License: " <+> text (show (license pkg))
     ])
     $+$ text ""
  where
    maybeShow [] _ _ = empty
    maybeShow l  s f = text s <+> (f l)

showPackageDetailedInfo :: PackageDisplayInfo -> String
showPackageDetailedInfo pkg =
  renderStyle (style {lineLength = 80, ribbonsPerLine = 1}) $
   char '*' <+> disp (pkgname pkg)
   $+$
   (nest 4 $ vcat [
     entry "Latest version available" availableVersions
           (altText "[ Not available from server ]")
           (disp . maximum)
   , entry "Latest version installed" installedVersions
           (altText "[ Not installed ]") --FIXME: unknown for non-libs
           (disp . maximum)
   , entry "Homepage"      homepage     orNotSpecified text             
   , entry "Bug reports"   bugReports   orNotSpecified text             
   , entry "Description"   description  alwaysShow     reflowParagraphs 
   , entry "Category"      category     hideIfNull     text             
   , entry "License"       license      alwaysShow     disp
   , entry "Author"        author       hideIfNull     reflowLines      
   , entry "Maintainer"    maintainer   hideIfNull     reflowLines      
   , entry "Source repo"   sourceRepo   orNotSpecified text             
   , entry "Executables"   executables  hideIfNull     (commaSep text)  
   , entry "Flags"         flags        hideIfNull     (commaSep dispFlag)
   , entry "Dependencies"  dependencies hideIfNull     (commaSep disp)
   , entry "Documentation" haddockHtml  showIfInstalled text
   , entry "Downloaded"    haveTarball  alwaysShow     dispYesNo
   , text "Modules:" $+$ nest 4 (vcat (map disp . sort . modules $ pkg))
   ])
   $+$ text ""
  where
    entry fname field cond format = case cond (field pkg) of
      Nothing           -> label <+> format (field pkg)
      Just Nothing      -> empty
      Just (Just other) -> label <+> text other
      where
        label   = text fname <> char ':' <> padding
        padding = text (replicate (13 - length fname ) ' ')

    normal      = Nothing
    hide        = Just Nothing
    replace msg = Just (Just msg)
    
    alwaysShow = const normal
    hideIfNull v = if null v then hide else normal
    showIfInstalled v
      | not isInstalled = hide
      | null v          = replace "[ Not installed ]"
      | otherwise       = normal
    altText msg v = if null v then replace msg else normal
    orNotSpecified = altText "[ Not specified ]"
    
    commaSep f = Disp.fsep . Disp.punctuate (Disp.char ',') . map f
    dispFlag f = case flagName f of FlagName n -> text n
    dispYesNo True  = text "Yes"
    dispYesNo False = text "No"

    isInstalled = not (null (installedVersions pkg))
--    hasLibs = --TODO
--    hasExes = --TODO

reflowParagraphs :: String -> Doc
reflowParagraphs =
    vcat
  . intersperse (text "")                    -- re-insert blank lines
  . map (fsep . map text . concatMap words)  -- reflow paragraphs
  . filter (/= [""])
  . groupBy (\x y -> "" `notElem` [x,y])     -- break on blank lines
  . lines

reflowLines :: String -> Doc
reflowLines = vcat . map text . lines

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
    pkgname      = combine packageName latestAvailable
                           packageName latestInstalled,
    installedVersions = map packageVersion installed,
    availableVersions = map packageVersion available,
    license      = combine Available.license  latestAvailableDesc
                           Installed.license latestInstalled,
    maintainer   = combine Available.maintainer latestAvailableDesc
                           Installed.maintainer latestInstalled,
    author       = combine Available.author latestAvailableDesc
                           Installed.author latestInstalled,
    homepage     = combine Available.homepage latestAvailableDesc
                           Installed.homepage latestInstalled,
    bugReports   = maybe "" Available.bugReports latestAvailableDesc,
    sourceRepo   = fromMaybe "" . join
                 . fmap (uncons Nothing Available.repoLocation
                       . sortBy (comparing Available.repoKind)
                       . Available.sourceRepos)
                 $ latestAvailableDesc,
    synopsis     = combine Available.synopsis latestAvailableDesc
                           Installed.description latestInstalled,
    description  = combine Available.description latestAvailableDesc
                           Installed.description latestInstalled,
    category     = combine Available.category latestAvailableDesc
                           Installed.category latestInstalled,
    flags        = maybe [] Available.genPackageFlags latestAvailable,
    executables  = map fst (maybe [] Available.condExecutables latestAvailable),
    modules      = combine Installed.exposedModules latestInstalled
                           (maybe [] Available.exposedModules
                               . Available.library) latestAvailableDesc,
    dependencies = combine Available.buildDepends latestAvailableDesc
                           (map thisPackageVersion
                             . Installed.depends) latestInstalled,
    haddockHtml  = fromMaybe "" . join
                 . fmap (listToMaybe . Installed.haddockHTMLs)
                 $ latestInstalled,
    haveTarball  = False
  }
  where
    combine f x g y = fromJust (fmap f x `mplus` fmap g y)
    latestInstalled = latestOf installed
    latestAvailable = packageDescription `fmap` latestOf available
    latestAvailableDesc = fmap flattenPackageDescription latestAvailable
    latestOf :: Package pkg => [pkg] -> Maybe pkg
    latestOf []   = Nothing
    latestOf pkgs = Just (maximumBy (comparing packageVersion) pkgs)
    
    uncons :: b -> (a -> b) -> [a] -> b
    uncons z _ []    = z
    uncons _ f (x:_) = f x

-- | Not all the info is pure. We have to check if the docs really are
-- installed, because the registered package info lies. Similarly we have to
-- check if the tarball has indeed been fetched.
--
updateFileSystemPackageDetails :: PackageDisplayInfo -> IO PackageDisplayInfo
updateFileSystemPackageDetails = return --FIXME

-- | Rearrange installed and available packages into groups referring to the
-- same package by name. In the result pairs, the lists are guaranteed to not
-- both be empty.
--
mergePackages ::   [InstalledPackageInfo] -> [AvailablePackage]
              -> [([InstalledPackageInfo],   [AvailablePackage])]
mergePackages installed available =
    map collect
  $ mergeBy (\i a -> fst i `compare` fst a)
            (groupOn packageName installed)
            (groupOn packageName available)
  where
    collect (OnlyInLeft  (_,is)       ) = (is, [])
    collect (    InBoth  (_,is) (_,as)) = (is, as)
    collect (OnlyInRight        (_,as)) = ([], as)

groupOn :: Ord key => (a -> key) -> [a] -> [(key,[a])]
groupOn key = map (\xs -> (key (head xs), xs))
            . groupBy (equating key)
            . sortBy (comparing key)
