{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.List
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2008-2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
--
-- Search for and print information about packages
module Distribution.Client.List
  ( list
  , info
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.License (License)
import Distribution.ModuleName (ModuleName)
import Distribution.Package
  ( Package (..)
  , PackageName
  , UnitId
  , packageName
  , packageVersion
  )
import Distribution.PackageDescription
  ( PackageFlag (..)
  , unFlagName
  )
import qualified Distribution.PackageDescription as Source
import Distribution.PackageDescription.Configuration
  ( flattenPackageDescription
  )
import Distribution.Types.Dependency
import Distribution.Types.UnqualComponentName

import Distribution.Simple.Compiler
  ( Compiler
  , PackageDBStack
  )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import Distribution.Simple.Program (ProgramDb)
import Distribution.Simple.Setup (fromFlag, fromFlagOrDefault)
import Distribution.Simple.Utils
  ( dieWithException
  , equating
  , notice
  )
import Distribution.Version
  ( Version
  , VersionRange
  , anyVersion
  , intersectVersionRanges
  , mkVersion
  , simplifyVersionRange
  , versionNumbers
  , withinRange
  )

import qualified Distribution.SPDX as SPDX

import Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Solver.Types.SourcePackage

import Distribution.Client.FetchUtils
  ( isFetched
  )
import Distribution.Client.IndexUtils as IndexUtils
  ( getInstalledPackages
  , getSourcePackages
  )
import Distribution.Client.Setup
  ( GlobalFlags (..)
  , InfoFlags (..)
  , ListFlags (..)
  , RepoContext (..)
  )
import Distribution.Client.Targets
  ( UserTarget
  , resolveUserTargets
  )
import Distribution.Client.Types
  ( PackageSpecifier (..)
  , SourcePackageDb (..)
  , UnresolvedSourcePackage
  )
import Distribution.Client.Utils
  ( MergeResult (..)
  , mergeBy
  )

import Control.Exception
  ( assert
  )
import Data.Bits ((.|.))
import Data.List
  ( maximumBy
  )
import qualified Data.List as L
import Data.List.NonEmpty (groupBy)
import qualified Data.Map as Map
import Data.Maybe
  ( fromJust
  )
import Data.Tree as Tree
import System.Directory
  ( doesDirectoryExist
  )
import Text.PrettyPrint
  ( Doc
  , char
  , fsep
  , lineLength
  , nest
  , parens
  , renderStyle
  , ribbonsPerLine
  , style
  , text
  , vcat
  , ($+$)
  )
import qualified Text.PrettyPrint as Disp

import Distribution.Client.Errors
import Distribution.Utils.ShortText (ShortText)
import qualified Distribution.Utils.ShortText as ShortText
import qualified Text.Regex.Base as Regex
import qualified Text.Regex.Posix.String as Regex

-- | Return a list of packages matching given search strings.
getPkgList
  :: Verbosity
  -> PackageDBStack
  -> RepoContext
  -> Maybe (Compiler, ProgramDb)
  -> ListFlags
  -> [String]
  -> IO [PackageDisplayInfo]
getPkgList verbosity packageDBs repoCtxt mcompprogdb listFlags pats = do
  installedPkgIndex <- for mcompprogdb $ \(comp, progdb) ->
    getInstalledPackages verbosity comp packageDBs progdb
  sourcePkgDb <- getSourcePackages verbosity repoCtxt

  regexps <- for pats $ \pat -> do
    e <- Regex.compile compOption Regex.execBlank pat
    case e of
      Right r -> return r
      Left err -> dieWithException verbosity $ GetPkgList pat err

  let sourcePkgIndex = packageIndex sourcePkgDb
      prefs name =
        fromMaybe
          anyVersion
          (Map.lookup name (packagePreferences sourcePkgDb))

      pkgsInfoMatching
        :: [(PackageName, [Installed.InstalledPackageInfo], [UnresolvedSourcePackage])]
      pkgsInfoMatching =
        let matchingInstalled = maybe [] (matchingPackages InstalledPackageIndex.searchWithPredicate regexps) installedPkgIndex
            matchingSource = matchingPackages (\idx n -> concatMap snd (PackageIndex.searchWithPredicate idx n)) regexps sourcePkgIndex
         in mergePackages matchingInstalled matchingSource

      pkgsInfo
        :: [(PackageName, [Installed.InstalledPackageInfo], [UnresolvedSourcePackage])]
      pkgsInfo
        -- gather info for all packages
        | null regexps =
            mergePackages
              (maybe [] InstalledPackageIndex.allPackages installedPkgIndex)
              (PackageIndex.allPackages sourcePkgIndex)
        -- gather info for packages matching search term
        | otherwise = pkgsInfoMatching

      matches :: [PackageDisplayInfo]
      matches =
        [ mergePackageInfo
          pref
          installedPkgs
          sourcePkgs
          selectedPkg
          False
        | (pkgname, installedPkgs, sourcePkgs) <- pkgsInfo
        , not onlyInstalled || not (null installedPkgs)
        , let pref = prefs pkgname
              selectedPkg = latestWithPref pref sourcePkgs
        ]
  return matches
  where
    onlyInstalled = fromFlagOrDefault False (listInstalled listFlags)
    caseInsensitive = fromFlagOrDefault True (listCaseInsensitive listFlags)

    compOption
      | caseInsensitive = Regex.compExtended .|. Regex.compIgnoreCase
      | otherwise = Regex.compExtended

    matchingPackages search regexps index =
      [ pkg
      | re <- regexps
      , pkg <- search index (Regex.matchTest re)
      ]

-- | Show information about packages.
list
  :: Verbosity
  -> PackageDBStack
  -> RepoContext
  -> Maybe (Compiler, ProgramDb)
  -> ListFlags
  -> [String]
  -> IO ()
list verbosity packageDBs repos mcompProgdb listFlags pats = do
  matches <- getPkgList verbosity packageDBs repos mcompProgdb listFlags pats

  if simpleOutput
    then
      putStr $
        unlines
          [ prettyShow (pkgName pkg) ++ " " ++ prettyShow version
          | pkg <- matches
          , version <-
              if onlyInstalled
                then installedVersions pkg
                else
                  nub . sort $
                    installedVersions pkg
                      ++ sourceVersions pkg
          ]
    else -- Note: this only works because for 'list', one cannot currently
    -- specify any version constraints, so listing all installed
    -- and source ones works.

      if null matches
        then notice verbosity "No matches found."
        else putStr $ unlines (map showPackageSummaryInfo matches)
  where
    onlyInstalled = fromFlag (listInstalled listFlags)
    simpleOutput = fromFlag (listSimpleOutput listFlags)

info
  :: Verbosity
  -> PackageDBStack
  -> RepoContext
  -> Compiler
  -> ProgramDb
  -> GlobalFlags
  -> InfoFlags
  -> [UserTarget]
  -> IO ()
info verbosity _ _ _ _ _ _ [] =
  notice verbosity "No packages requested. Nothing to do."
info
  verbosity
  packageDBs
  repoCtxt
  comp
  progdb
  _
  _listFlags
  userTargets = do
    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
    sourcePkgDb <- getSourcePackages verbosity repoCtxt
    let sourcePkgIndex = packageIndex sourcePkgDb
        prefs name =
          fromMaybe
            anyVersion
            (Map.lookup name (packagePreferences sourcePkgDb))

    -- Users may specify names of packages that are only installed, not
    -- just available source packages, so we must resolve targets using
    -- the combination of installed and source packages.
    let sourcePkgs' =
          PackageIndex.fromList $
            map
              packageId
              (InstalledPackageIndex.allPackages installedPkgIndex)
              ++ map
                packageId
                (PackageIndex.allPackages sourcePkgIndex)
    pkgSpecifiers <-
      resolveUserTargets
        verbosity
        repoCtxt
        sourcePkgs'
        userTargets

    pkgsinfo <-
      sequenceA
        [ do
          pkginfo <-
            either (dieWithException verbosity) return $
              gatherPkgInfo
                prefs
                installedPkgIndex
                sourcePkgIndex
                pkgSpecifier
          updateFileSystemPackageDetails pkginfo
        | pkgSpecifier <- pkgSpecifiers
        ]

    putStr $ unlines (map showPackageDetailedInfo pkgsinfo)
    where
      gatherPkgInfo
        :: (PackageName -> VersionRange)
        -> InstalledPackageIndex
        -> PackageIndex.PackageIndex UnresolvedSourcePackage
        -> PackageSpecifier UnresolvedSourcePackage
        -> Either CabalInstallException PackageDisplayInfo
      gatherPkgInfo
        prefs
        installedPkgIndex
        sourcePkgIndex
        (NamedPackage name props)
          | null (selectedInstalledPkgs) && null (selectedSourcePkgs) =
              Left $ GatherPkgInfo name (simplifyVersionRange verConstraint)
          | otherwise =
              Right $
                mergePackageInfo
                  pref
                  installedPkgs
                  sourcePkgs
                  selectedSourcePkg'
                  showPkgVersion
          where
            (pref, installedPkgs, sourcePkgs) =
              sourcePkgsInfo prefs name installedPkgIndex sourcePkgIndex

            selectedInstalledPkgs =
              InstalledPackageIndex.lookupDependency
                installedPkgIndex
                name
                verConstraint
            selectedSourcePkgs =
              PackageIndex.lookupDependency
                sourcePkgIndex
                name
                verConstraint
            selectedSourcePkg' = latestWithPref pref selectedSourcePkgs

            -- display a specific package version if the user
            -- supplied a non-trivial version constraint
            showPkgVersion = not (null verConstraints)
            verConstraint = foldr intersectVersionRanges anyVersion verConstraints
            verConstraints = [vr | PackagePropertyVersion vr <- props]
      gatherPkgInfo
        prefs
        installedPkgIndex
        sourcePkgIndex
        (SpecificSourcePackage pkg) =
          Right $
            mergePackageInfo
              pref
              installedPkgs
              sourcePkgs
              selectedPkg
              True
          where
            name = packageName pkg
            selectedPkg = Just pkg
            (pref, installedPkgs, sourcePkgs) =
              sourcePkgsInfo prefs name installedPkgIndex sourcePkgIndex

sourcePkgsInfo
  :: (PackageName -> VersionRange)
  -> PackageName
  -> InstalledPackageIndex
  -> PackageIndex.PackageIndex UnresolvedSourcePackage
  -> (VersionRange, [Installed.InstalledPackageInfo], [UnresolvedSourcePackage])
sourcePkgsInfo prefs name installedPkgIndex sourcePkgIndex =
  (pref, installedPkgs, sourcePkgs)
  where
    pref = prefs name
    installedPkgs =
      concatMap
        snd
        ( InstalledPackageIndex.lookupPackageName
            installedPkgIndex
            name
        )
    sourcePkgs = PackageIndex.lookupPackageName sourcePkgIndex name

-- | The info that we can display for each package. It is information per
-- package name and covers all installed and available versions.
data PackageDisplayInfo = PackageDisplayInfo
  { pkgName :: PackageName
  , selectedVersion :: Maybe Version
  , selectedSourcePkg :: Maybe UnresolvedSourcePackage
  , installedVersions :: [Version]
  , sourceVersions :: [Version]
  , preferredVersions :: VersionRange
  , homepage :: ShortText
  , bugReports :: ShortText
  , sourceRepo :: String -- TODO
  , synopsis :: ShortText
  , description :: ShortText
  , category :: ShortText
  , license :: Either SPDX.License License
  , author :: ShortText
  , maintainer :: ShortText
  , dependencies :: [ExtDependency]
  , flags :: [PackageFlag]
  , hasLib :: Bool
  , hasExe :: Bool
  , executables :: [UnqualComponentName]
  , modules :: [ModuleName]
  , haddockHtml :: FilePath
  , haveTarball :: Bool
  }

-- | Covers source dependencies and installed dependencies in
-- one type.
data ExtDependency
  = SourceDependency Dependency
  | InstalledDependency UnitId

showPackageSummaryInfo :: PackageDisplayInfo -> String
showPackageSummaryInfo pkginfo =
  renderStyle (style{lineLength = 80, ribbonsPerLine = 1}) $
    char '*'
      <+> pretty (pkgName pkginfo)
      $+$ nest
        4
        ( vcat
            [ maybeShowST (synopsis pkginfo) "Synopsis:" reflowParagraphs
            , text "Default available version:"
                <+> case selectedSourcePkg pkginfo of
                  Nothing -> text "[ Not available from any configured repository ]"
                  Just pkg -> pretty (packageVersion pkg)
            , text "Installed versions:"
                <+> case installedVersions pkginfo of
                  []
                    | hasLib pkginfo -> text "[ Not installed ]"
                    | otherwise -> text "[ Unknown ]"
                  versions ->
                    dispTopVersions
                      4
                      (preferredVersions pkginfo)
                      versions
            , maybeShowST (homepage pkginfo) "Homepage:" text
            , text "License: " <+> either pretty pretty (license pkginfo)
            ]
        )
      $+$ text ""
  where
    maybeShowST l s f
      | ShortText.null l = Disp.empty
      | otherwise = text s <+> f (ShortText.fromShortText l)

showPackageDetailedInfo :: PackageDisplayInfo -> String
showPackageDetailedInfo pkginfo =
  renderStyle (style{lineLength = 80, ribbonsPerLine = 1}) $
    char '*'
      <+> pretty (pkgName pkginfo)
        <<>> maybe Disp.empty (\v -> char '-' Disp.<> pretty v) (selectedVersion pkginfo)
      <+> text (replicate (16 - length (prettyShow (pkgName pkginfo))) ' ')
        <<>> parens pkgkind
      $+$ nest
        4
        ( vcat
            [ entryST "Synopsis" synopsis hideIfNull reflowParagraphs
            , entry
                "Versions available"
                sourceVersions
                (altText null "[ Not available from server ]")
                (dispTopVersions 9 (preferredVersions pkginfo))
            , entry
                "Versions installed"
                installedVersions
                ( altText
                    null
                    ( if hasLib pkginfo
                        then "[ Not installed ]"
                        else "[ Unknown ]"
                    )
                )
                (dispTopVersions 4 (preferredVersions pkginfo))
            , entryST "Homepage" homepage orNotSpecified text
            , entryST "Bug reports" bugReports orNotSpecified text
            , entryST "Description" description hideIfNull reflowParagraphs
            , entryST "Category" category hideIfNull text
            , entry "License" license alwaysShow (either pretty pretty)
            , entryST "Author" author hideIfNull reflowLines
            , entryST "Maintainer" maintainer hideIfNull reflowLines
            , entry "Source repo" sourceRepo orNotSpecified text
            , entry "Executables" executables hideIfNull (commaSep pretty)
            , entry "Flags" flags hideIfNull (commaSep dispFlag)
            , entry "Dependencies" dependencies hideIfNull (commaSep dispExtDep)
            , entry "Documentation" haddockHtml showIfInstalled text
            , entry "Cached" haveTarball alwaysShow dispYesNo
            , if not (hasLib pkginfo)
                then mempty
                else text "Modules:" $+$ nest 4 (vcat (map pretty . sort . modules $ pkginfo))
            ]
        )
      $+$ text ""
  where
    entry fname field cond format = case cond (field pkginfo) of
      Nothing -> label <+> format (field pkginfo)
      Just Nothing -> mempty
      Just (Just other) -> label <+> text other
      where
        label = text fname Disp.<> char ':' Disp.<> padding
        padding = text (replicate (13 - length fname) ' ')

    entryST fname field = entry fname (ShortText.fromShortText . field)

    normal = Nothing
    hide = Just Nothing
    replace msg = Just (Just msg)

    alwaysShow = const normal
    hideIfNull v = if null v then hide else normal
    showIfInstalled v
      | not isInstalled = hide
      | null v = replace "[ Not installed ]"
      | otherwise = normal
    altText nul msg v = if nul v then replace msg else normal
    orNotSpecified = altText null "[ Not specified ]"

    commaSep f = Disp.fsep . Disp.punctuate (Disp.char ',') . map f
    dispFlag = text . unFlagName . flagName
    dispYesNo True = text "Yes"
    dispYesNo False = text "No"

    dispExtDep (SourceDependency dep) = pretty dep
    dispExtDep (InstalledDependency dep) = pretty dep

    isInstalled = not (null (installedVersions pkginfo))
    hasExes = length (executables pkginfo) >= 2
    -- TODO: exclude non-buildable exes
    pkgkind
      | hasLib pkginfo && hasExes = text "programs and library"
      | hasLib pkginfo && hasExe pkginfo = text "program and library"
      | hasLib pkginfo = text "library"
      | hasExes = text "programs"
      | hasExe pkginfo = text "program"
      | otherwise = mempty

reflowParagraphs :: String -> Doc
reflowParagraphs =
  vcat
    . intersperse (text "") -- re-insert blank lines
    . map (fsep . map text . concatMap words) -- reflow paragraphs
    . filter (/= [""])
    . L.groupBy (\x y -> "" `notElem` [x, y]) -- break on blank lines
    . lines

reflowLines :: String -> Doc
reflowLines = vcat . map text . lines

-- | We get the 'PackageDisplayInfo' by combining the info for the installed
-- and available versions of a package.
--
-- * We're building info about a various versions of a single named package so
-- the input package info records are all supposed to refer to the same
-- package name.
mergePackageInfo
  :: VersionRange
  -> [Installed.InstalledPackageInfo]
  -> [UnresolvedSourcePackage]
  -> Maybe UnresolvedSourcePackage
  -> Bool
  -> PackageDisplayInfo
mergePackageInfo versionPref installedPkgs sourcePkgs selectedPkg showVer =
  assert (length installedPkgs + length sourcePkgs > 0) $
    PackageDisplayInfo
      { pkgName =
          combine
            packageName
            source
            packageName
            installed
      , selectedVersion =
          if showVer
            then fmap packageVersion selectedPkg
            else Nothing
      , selectedSourcePkg = sourceSelected
      , installedVersions = map packageVersion installedPkgs
      , sourceVersions = map packageVersion sourcePkgs
      , preferredVersions = versionPref
      , license =
          combine
            Source.licenseRaw
            source
            Installed.license
            installed
      , maintainer =
          combine
            Source.maintainer
            source
            Installed.maintainer
            installed
      , author =
          combine
            Source.author
            source
            Installed.author
            installed
      , homepage =
          combine
            Source.homepage
            source
            Installed.homepage
            installed
      , bugReports = maybe mempty Source.bugReports source
      , sourceRepo =
          fromMaybe mempty
            . join
            . fmap
              ( uncons Nothing Source.repoLocation
                  . sortBy (comparing Source.repoKind)
                  . Source.sourceRepos
              )
            $ source
      , -- TODO: installed package info is missing synopsis
        synopsis = maybe mempty Source.synopsis source
      , description =
          combine
            Source.description
            source
            Installed.description
            installed
      , category =
          combine
            Source.category
            source
            Installed.category
            installed
      , flags = maybe [] Source.genPackageFlags sourceGeneric
      , hasLib =
          isJust installed
            || maybe False (isJust . Source.condLibrary) sourceGeneric
      , hasExe = maybe False (not . null . Source.condExecutables) sourceGeneric
      , executables = map fst (maybe [] Source.condExecutables sourceGeneric)
      , modules =
          combine
            (map Installed.exposedName . Installed.exposedModules)
            installed
            -- NB: only for the PUBLIC library
            (concatMap getListOfExposedModules . maybeToList . Source.library)
            source
      , dependencies =
          combine
            ( map (SourceDependency . simplifyDependency)
                . Source.allBuildDepends
            )
            source
            (map InstalledDependency . Installed.depends)
            installed
      , haddockHtml =
          fromMaybe ""
            . join
            . fmap (listToMaybe . Installed.haddockHTMLs)
            $ installed
      , haveTarball = False
      }
  where
    combine f x g y = fromJust (fmap f x `mplus` fmap g y)
    installed :: Maybe Installed.InstalledPackageInfo
    installed = latestWithPref versionPref installedPkgs

    getListOfExposedModules lib =
      Source.exposedModules lib
        ++ map
          Source.moduleReexportName
          (Source.reexportedModules lib)

    sourceSelected
      | isJust selectedPkg = selectedPkg
      | otherwise = latestWithPref versionPref sourcePkgs
    sourceGeneric = fmap srcpkgDescription sourceSelected
    source = fmap flattenPackageDescription sourceGeneric

    uncons :: b -> (a -> b) -> [a] -> b
    uncons z _ [] = z
    uncons _ f (x : _) = f x

-- | Not all the info is pure. We have to check if the docs really are
-- installed, because the registered package info lies. Similarly we have to
-- check if the tarball has indeed been fetched.
updateFileSystemPackageDetails :: PackageDisplayInfo -> IO PackageDisplayInfo
updateFileSystemPackageDetails pkginfo = do
  fetched <-
    maybe
      (return False)
      (isFetched . srcpkgSource)
      (selectedSourcePkg pkginfo)
  docsExist <- doesDirectoryExist (haddockHtml pkginfo)
  return
    pkginfo
      { haveTarball = fetched
      , haddockHtml = if docsExist then haddockHtml pkginfo else ""
      }

latestWithPref :: Package pkg => VersionRange -> [pkg] -> Maybe pkg
latestWithPref _ [] = Nothing
latestWithPref pref pkgs = Just (maximumBy (comparing prefThenVersion) pkgs)
  where
    prefThenVersion pkg =
      let ver = packageVersion pkg
       in (withinRange ver pref, ver)

-- | Rearrange installed and source packages into groups referring to the
-- same package by name. In the result pairs, the lists are guaranteed to not
-- both be empty.
mergePackages
  :: [Installed.InstalledPackageInfo]
  -> [UnresolvedSourcePackage]
  -> [ ( PackageName
       , [Installed.InstalledPackageInfo]
       , [UnresolvedSourcePackage]
       )
     ]
mergePackages installedPkgs sourcePkgs =
  map collect $
    mergeBy
      (\i a -> fst i `compare` fst a)
      (groupOn packageName installedPkgs)
      (groupOn packageName sourcePkgs)
  where
    collect (OnlyInLeft (name, is)) = (name, is, [])
    collect (InBoth (_, is) (name, as)) = (name, is, as)
    collect (OnlyInRight (name, as)) = (name, [], as)

groupOn :: Ord key => (a -> key) -> [a] -> [(key, [a])]
groupOn key =
  map (\xs -> (key (head xs), toList xs))
    . groupBy (equating key)
    . sortBy (comparing key)

dispTopVersions :: Int -> VersionRange -> [Version] -> Doc
dispTopVersions n pref vs =
  ( Disp.fsep
      . Disp.punctuate (Disp.char ',')
      . map (\ver -> if ispref ver then pretty ver else parens (pretty ver))
      . sort
      . take n
      . interestingVersions ispref
      $ vs
  )
    <+> trailingMessage
  where
    ispref ver = withinRange ver pref
    extra = length vs - n
    trailingMessage
      | extra <= 0 = Disp.empty
      | otherwise =
          Disp.parens $
            Disp.text "and"
              <+> Disp.int (length vs - n)
              <+> if extra == 1
                then Disp.text "other"
                else Disp.text "others"

-- | Reorder a bunch of versions to put the most interesting / significant
-- versions first. A preferred version range is taken into account.
--
-- This may be used in a user interface to select a small number of versions
-- to present to the user, e.g.
--
-- > let selectVersions = sort . take 5 . interestingVersions pref
interestingVersions :: (Version -> Bool) -> [Version] -> [Version]
interestingVersions pref =
  map (mkVersion . fst)
    . filter snd
    . concat
    . Tree.levels
    . swizzleTree
    . reorderTree (\(Node (v, _) _) -> pref (mkVersion v))
    . reverseTree
    . mkTree
    . map (or0 . versionNumbers)
  where
    or0 [] = 0 :| []
    or0 (x : xs) = x :| xs

    swizzleTree = unfoldTree (spine [])
      where
        spine ts' (Node x []) = (x, ts')
        spine ts' (Node x (t : ts)) = spine (Node x ts : ts') t

    reorderTree _ (Node x []) = Node x []
    reorderTree p (Node x ts) = Node x (ts' ++ ts'')
      where
        (ts', ts'') = partition p (map (reorderTree p) ts)

    reverseTree (Node x cs) = Node x (reverse (map reverseTree cs))

    mkTree :: forall a. Eq a => [NonEmpty a] -> Tree ([a], Bool)
    mkTree xs = unfoldTree step (False, [], xs)
      where
        step :: (Bool, [a], [NonEmpty a]) -> (([a], Bool), [(Bool, [a], [NonEmpty a])])
        step (node, ns, vs) =
          ( (reverse ns, node)
          , [ (any null vs', n : ns, mapMaybe nonEmpty (toList vs'))
            | (n, vs') <- groups vs
            ]
          )

        groups :: [NonEmpty a] -> [(a, NonEmpty [a])]
        groups =
          map (\g -> (head (head g), fmap tail g))
            . groupBy (equating head)
