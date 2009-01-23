-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.List
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2008-2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
--
-- Search for and print information about packages
-----------------------------------------------------------------------------
module Distribution.Client.List (
  list, info
  ) where

import Distribution.Package
         ( PackageName(..), packageName, packageVersion
         , Dependency(..), thisPackageVersion )
import Distribution.ModuleName (ModuleName)
import Distribution.License (License)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.PackageDescription   as Available
import Distribution.PackageDescription
         ( Flag(..), FlagName(..) )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )

import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Compiler (Compiler,PackageDB)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Utils (equating, comparing, notice)
import Distribution.Simple.Setup (fromFlag)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Version   (Version)
import Distribution.Verbosity (Verbosity)
import Distribution.Text
         ( Text(disp), display )

import Distribution.Client.Types
         ( AvailablePackage(..), Repo, AvailablePackageDb(..)
         , UnresolvedDependency(..) )
import Distribution.Client.Setup
         ( ListFlags(..), InfoFlags(..) )
import Distribution.Client.Utils
         ( mergeBy, MergeResult(..) )
import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages, disambiguateDependencies )
import Distribution.Client.Fetch
         ( isFetched )

import Data.List
         ( sortBy, groupBy, sort, nub, intersperse, maximumBy )
import Data.Maybe
         ( listToMaybe, fromJust, fromMaybe, isJust, isNothing )
import Control.Monad
         ( MonadPlus(mplus), join )
import Control.Exception
         ( assert )
import Text.PrettyPrint.HughesPJ as Disp
import System.Directory
         ( doesDirectoryExist )


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
    allInstalled      :: [InstalledPackageInfo],
    allAvailable      :: [AvailablePackage],
    latestInstalled   :: Maybe InstalledPackageInfo,
    latestAvailable   :: Maybe AvailablePackage,
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
    hasLib            :: Bool,
    hasExe            :: Bool,
    executables       :: [String],
    modules           :: [ModuleName],
    haddockHtml       :: FilePath,
    haveTarball       :: Bool
  }

installedVersions :: PackageDisplayInfo -> [Version]
installedVersions = map packageVersion . allInstalled

availableVersions :: PackageDisplayInfo -> [Version]
availableVersions = map packageVersion . allAvailable

showPackageSummaryInfo :: PackageDisplayInfo -> String
showPackageSummaryInfo pkginfo =
  renderStyle (style {lineLength = 80, ribbonsPerLine = 1}) $
     char '*' <+> disp (pkgname pkginfo)
     $+$
     (nest 4 $ vcat [
       maybeShow (synopsis pkginfo) "Synopsis:" reflowParagraphs
     , text "Latest version available:" <+>
       case latestAvailable pkginfo of
         Nothing  -> text "[ Not available from server ]"
         Just pkg -> disp (packageVersion pkg)
     , text "Latest version installed:" <+>
       case latestInstalled pkginfo of
         Nothing  -> text "[ Not installed ]"
         Just pkg -> disp (packageVersion pkg)
     , maybeShow (homepage pkginfo) "Homepage:" text
     , text "License: " <+> text (show (license pkginfo))
     ])
     $+$ text ""
  where
    maybeShow [] _ _ = empty
    maybeShow l  s f = text s <+> (f l)

showPackageDetailedInfo :: PackageDisplayInfo -> String
showPackageDetailedInfo pkginfo =
  renderStyle (style {lineLength = 80, ribbonsPerLine = 1}) $
   char '*' <+> disp (pkgname pkginfo)
            <+> text (replicate (16 - length (display (pkgname pkginfo))) ' ')
            <>  parens pkgkind
   $+$
   (nest 4 $ vcat [
     entry "Synopsis"      synopsis     alwaysShow     reflowParagraphs
   , entry "Latest version available" latestAvailable
           (altText isNothing "[ Not available from server ]")
           (disp . packageVersion . fromJust)
   , entry "Latest version installed" latestInstalled
           (altText isNothing (if hasLib pkginfo then "[ Not installed ]"
                                                 else "[ Unknown ]"))
           (disp . packageVersion . fromJust)
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
   , entry "Cached"        haveTarball  alwaysShow     dispYesNo
   , if not (hasLib pkginfo) then empty else
     text "Modules:" $+$ nest 4 (vcat (map disp . sort . modules $ pkginfo))
   ])
   $+$ text ""
  where
    entry fname field cond format = case cond (field pkginfo) of
      Nothing           -> label <+> format (field pkginfo)
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
    altText nul msg v = if nul v then replace msg else normal
    orNotSpecified = altText null "[ Not specified ]"

    commaSep f = Disp.fsep . Disp.punctuate (Disp.char ',') . map f
    dispFlag f = case flagName f of FlagName n -> text n
    dispYesNo True  = text "Yes"
    dispYesNo False = text "No"

    isInstalled = not (null (installedVersions pkginfo))
    hasExes = length (executables pkginfo) >= 2
    --TODO: exclude non-buildable exes
    pkgkind | hasLib pkginfo && hasExes        = text "programs and library"
            | hasLib pkginfo && hasExe pkginfo = text "program and library"
            | hasLib pkginfo                   = text "library"
            | hasExes                          = text "programs"
            | hasExe pkginfo                   = text "program"
            | otherwise                        = empty

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
mergePackageInfo installedPkgs availablePkgs =
  assert (length installedPkgs + length availablePkgs > 0) $
  PackageDisplayInfo {
    pkgname      = combine packageName available
                           packageName installed,
    allInstalled = installedPkgs,
    allAvailable = availablePkgs,
    latestInstalled = latest installedPkgs,
    latestAvailable = latest availablePkgs,
    license      = combine Available.license    available
                           Installed.license    installed,
    maintainer   = combine Available.maintainer available
                           Installed.maintainer installed,
    author       = combine Available.author     available
                           Installed.author     installed,
    homepage     = combine Available.homepage   available
                           Installed.homepage   installed,
    bugReports   = maybe "" Available.bugReports available,
    sourceRepo   = fromMaybe "" . join
                 . fmap (uncons Nothing Available.repoLocation
                       . sortBy (comparing Available.repoKind)
                       . Available.sourceRepos)
                 $ available,
    synopsis     = combine Available.synopsis    available
                           Installed.description installed,
    description  = combine Available.description available
                           Installed.description installed,
    category     = combine Available.category    available
                           Installed.category    installed,
    flags        = maybe [] Available.genPackageFlags availableGeneric,
    hasLib       = isJust installed
                || fromMaybe False
                   (fmap (isJust . Available.condLibrary) availableGeneric),
    hasExe       = fromMaybe False
                   (fmap (not . null . Available.condExecutables) availableGeneric),
    executables  = map fst (maybe [] Available.condExecutables availableGeneric),
    modules      = combine Installed.exposedModules installed
                           (maybe [] Available.exposedModules
                                   . Available.library) available,
    dependencies = combine Available.buildDepends available
                           (map thisPackageVersion
                             . Installed.depends) installed,
    haddockHtml  = fromMaybe "" . join
                 . fmap (listToMaybe . Installed.haddockHTMLs)
                 $ installed,
    haveTarball  = False
  }
  where
    combine f x g y  = fromJust (fmap f x `mplus` fmap g y)
    installed        = latest installedPkgs
    availableGeneric = fmap packageDescription (latest availablePkgs)
    available        = fmap flattenPackageDescription availableGeneric
    latest []        = Nothing
    latest pkgs      = Just (maximumBy (comparing packageVersion) pkgs)

    uncons :: b -> (a -> b) -> [a] -> b
    uncons z _ []    = z
    uncons _ f (x:_) = f x

-- | Not all the info is pure. We have to check if the docs really are
-- installed, because the registered package info lies. Similarly we have to
-- check if the tarball has indeed been fetched.
--
updateFileSystemPackageDetails :: PackageDisplayInfo -> IO PackageDisplayInfo
updateFileSystemPackageDetails pkginfo = do
  fetched   <- maybe (return False) isFetched (latestAvailable pkginfo)
  docsExist <- doesDirectoryExist (haddockHtml pkginfo)
  return pkginfo {
    haveTarball = fetched,
    haddockHtml = if docsExist then haddockHtml pkginfo else ""
  }

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
