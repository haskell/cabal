-- |
-- Module      :  Distribution.PackageDescription.Check.Target
-- Copyright   :  Lennart Kolmodin 2008, Francesco Ariis 2023
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Fully-realised target (library, executable, …) checking functions.
module Distribution.PackageDescription.Check.Target
  ( checkLibrary
  , checkForeignLib
  , checkExecutable
  , checkTestSuite
  , checkBenchmark
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compiler
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Check.Common
import Distribution.PackageDescription.Check.Monad
import Distribution.PackageDescription.Check.Paths
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildPaths
  ( autogenPackageInfoModuleName
  , autogenPathsModuleName
  )
import Distribution.Simple.Utils hiding (findPackageDesc, notice)
import Distribution.Types.PackageName.Magic
import Distribution.Utils.Path
import Distribution.Version
import Language.Haskell.Extension
import System.FilePath (takeExtension)

import Control.Monad

import qualified Distribution.Types.BuildInfo.Lens as L

checkLibrary
  :: Monad m
  => Bool -- Is this a sublibrary?
  -> [AssocDep] -- “Inherited” dependencies for PVP checks.
  -> Library
  -> CheckM m ()
checkLibrary
  isSub
  ads
  lib@( Library
          libName_
          _exposedModules_
          reexportedModules_
          signatures_
          _libExposed_
          _libVisibility_
          libBuildInfo_
        ) = do
    mapM_ checkModuleName (explicitLibModules lib)

    checkP
      (libName_ == LMainLibName && isSub)
      (PackageBuildImpossible UnnamedInternal)
    -- TODO: bogus if a required-signature was passed through.
    checkP
      (null (explicitLibModules lib) && null reexportedModules_)
      (PackageDistSuspiciousWarn (NoModulesExposed libName_))
    -- TODO parse-caught check, can safely remove.
    checkSpecVer
      CabalSpecV2_0
      (not . null $ signatures_)
      (PackageDistInexcusable SignaturesCabal2)
    -- autogen/includes checks.
    checkP
      ( not $
          all
            (flip elem (explicitLibModules lib))
            (libModulesAutogen lib)
      )
      (PackageBuildImpossible AutogenNotExposed)
    -- check that all autogen-includes appear on includes or
    -- install-includes.
    checkP
      ( not $
          all
            (flip elem (allExplicitIncludes lib) . getSymbolicPath)
            (view L.autogenIncludes lib)
      )
      $ (PackageBuildImpossible AutogenIncludesNotIncluded)

    -- § Build infos.
    checkBuildInfo
      (CETLibrary libName_)
      (explicitLibModules lib)
      ads
      libBuildInfo_

    -- Feature checks.
    -- check use of reexported-modules sections
    checkSpecVer
      CabalSpecV1_22
      (not . null $ reexportedModules_)
      (PackageDistInexcusable CVReexported)
    where
      allExplicitIncludes :: L.HasBuildInfo a => a -> [FilePath]
      allExplicitIncludes x =
        map getSymbolicPath (view L.includes x)
          ++ map getSymbolicPath (view L.installIncludes x)

checkForeignLib :: Monad m => ForeignLib -> CheckM m ()
checkForeignLib
  ( ForeignLib
      foreignLibName_
      _foreignLibType_
      _foreignLibOptions_
      foreignLibBuildInfo_
      _foreignLibVersionInfo_
      _foreignLibVersionLinux_
      _foreignLibModDefFile_
    ) = do
    checkBuildInfo
      (CETForeignLibrary foreignLibName_)
      []
      []
      foreignLibBuildInfo_

checkExecutable
  :: Monad m
  => [AssocDep] -- “Inherited” dependencies for PVP checks.
  -> Executable
  -> CheckM m ()
checkExecutable
  ads
  exe@( Executable
          exeName_
          symbolicModulePath_
          _exeScope_
          buildInfo_
        ) = do
    -- Target type/name (exe).
    let cet = CETExecutable exeName_
        modulePath_ = getSymbolicPath symbolicModulePath_

    mapM_ checkModuleName (exeModules exe)

    -- § Exe specific checks
    checkP
      (null modulePath_)
      (PackageBuildImpossible (NoMainIs exeName_))
    -- This check does not apply to scripts.
    pid <- asksCM (pnPackageId . ccNames)
    checkP
      ( pid /= fakePackageId
          && not (null modulePath_)
          && not (fileExtensionSupportedLanguage $ modulePath_)
      )
      (PackageBuildImpossible NoHsLhsMain)

    -- § Features check
    checkSpecVer
      CabalSpecV1_18
      ( fileExtensionSupportedLanguage modulePath_
          && takeExtension modulePath_ `notElem` [".hs", ".lhs"]
      )
      (PackageDistInexcusable MainCCabal1_18)

    -- Alas exeModules ad exeModulesAutogen (exported from
    -- Distribution.Types.Executable) take `Executable` as a parameter.
    checkP
      (not $ all (flip elem (exeModules exe)) (exeModulesAutogen exe))
      (PackageBuildImpossible $ AutogenNoOther cet)
    checkP
      ( not $
          all
            (flip elem (view L.includes exe) . relativeSymbolicPath)
            (view L.autogenIncludes exe)
      )
      (PackageBuildImpossible AutogenIncludesNotIncludedExe)

    -- § Build info checks.
    checkBuildInfo cet [] ads buildInfo_

checkTestSuite
  :: Monad m
  => [AssocDep] -- “Inherited” dependencies for PVP checks.
  -> TestSuite
  -> CheckM m ()
checkTestSuite
  ads
  ts@( TestSuite
        testName_
        testInterface_
        testBuildInfo_
        _testCodeGenerators_
      ) = do
    -- Target type/name (test).
    let cet = CETTest testName_

    -- § TS specific checks.
    -- TODO caught by the parser, can remove safely
    case testInterface_ of
      TestSuiteUnsupported tt@(TestTypeUnknown _ _) ->
        tellP (PackageBuildWarning $ TestsuiteTypeNotKnown tt)
      TestSuiteUnsupported tt ->
        tellP (PackageBuildWarning $ TestsuiteNotSupported tt)
      _ -> return ()

    mapM_ checkModuleName (testModules ts)

    checkP
      mainIsWrongExt
      (PackageBuildImpossible NoHsLhsMain)
    checkP
      ( not $
          all
            (flip elem (testModules ts))
            (testModulesAutogen ts)
      )
      (PackageBuildImpossible $ AutogenNoOther cet)
    checkP
      ( not $
          all
            (flip elem (view L.includes ts) . relativeSymbolicPath)
            (view L.autogenIncludes ts)
      )
      (PackageBuildImpossible AutogenIncludesNotIncludedExe)

    -- § Feature checks.
    checkSpecVer
      CabalSpecV1_18
      (mainIsNotHsExt && not mainIsWrongExt)
      (PackageDistInexcusable MainCCabal1_18)

    -- § Build info checks.
    checkBuildInfo cet [] ads testBuildInfo_
    where
      mainIsWrongExt =
        case testInterface_ of
          TestSuiteExeV10 _ f -> not (fileExtensionSupportedLanguage $ getSymbolicPath f)
          _ -> False

      mainIsNotHsExt =
        case testInterface_ of
          TestSuiteExeV10 _ f -> takeExtension (getSymbolicPath f) `notElem` [".hs", ".lhs"]
          _ -> False

checkBenchmark
  :: Monad m
  => [AssocDep] -- “Inherited” dependencies for PVP checks.
  -> Benchmark
  -> CheckM m ()
checkBenchmark
  ads
  bm@( Benchmark
        benchmarkName_
        benchmarkInterface_
        benchmarkBuildInfo_
      ) = do
    -- Target type/name (benchmark).
    let cet = CETBenchmark benchmarkName_

    mapM_ checkModuleName (benchmarkModules bm)

    -- § Interface & bm specific tests.
    case benchmarkInterface_ of
      BenchmarkUnsupported tt@(BenchmarkTypeUnknown _ _) ->
        tellP (PackageBuildWarning $ BenchmarkTypeNotKnown tt)
      BenchmarkUnsupported tt ->
        tellP (PackageBuildWarning $ BenchmarkNotSupported tt)
      _ -> return ()
    checkP
      mainIsWrongExt
      (PackageBuildImpossible NoHsLhsMainBench)

    checkP
      ( not $
          all
            (flip elem (benchmarkModules bm))
            (benchmarkModulesAutogen bm)
      )
      (PackageBuildImpossible $ AutogenNoOther cet)

    checkP
      ( not $
          all
            (flip elem (view L.includes bm) . relativeSymbolicPath)
            (view L.autogenIncludes bm)
      )
      (PackageBuildImpossible AutogenIncludesNotIncludedExe)

    -- § BuildInfo checks.
    checkBuildInfo cet [] ads benchmarkBuildInfo_
    where
      -- Cannot abstract with similar function in checkTestSuite,
      -- they are different.
      mainIsWrongExt =
        case benchmarkInterface_ of
          BenchmarkExeV10 _ f -> takeExtension (getSymbolicPath f) `notElem` [".hs", ".lhs"]
          _ -> False

-- | Check if a module name is valid on both Windows and Posix systems
checkModuleName :: Monad m => ModuleName -> CheckM m ()
checkModuleName moduleName =
  checkPackageFileNamesWithGlob PathKindFile (toFilePath moduleName)

-- ------------------------------------------------------------
-- Build info
-- ------------------------------------------------------------

-- Check a great deal of things in buildInfo.
-- With 'checkBuildInfo' we cannot follow the usual “pattern match
-- everything” method, for the number of BuildInfo fields (almost 50)
-- but more importantly because accessing options, etc. is done
-- with functions from 'Distribution.Types.BuildInfo' (e.g. 'hcOptions').
-- Duplicating the effort here means risk of diverging definitions for
-- little gain (most likely if a field is added to BI, the relevant
-- function will be tweaked in Distribution.Types.BuildInfo too).
checkBuildInfo
  :: Monad m
  => CEType -- Name and type of the target.
  -> [ModuleName] -- Additional module names which cannot be
  -- extracted from BuildInfo (mainly: exposed
  -- library modules).
  -> [AssocDep] -- Inherited “internal” (main lib, named
  -- internal libs) dependencies.
  -> BuildInfo
  -> CheckM m ()
checkBuildInfo cet ams ads bi = do
  -- For the sake of clarity, we split che checks in various
  -- (top level) functions, even if we are not actually going
  -- deeper in the traversal.

  checkBuildInfoOptions (cet2bit cet) bi
  checkBuildInfoPathsContent bi
  checkBuildInfoPathsWellFormedness bi

  sv <- asksCM ccSpecVersion
  checkBuildInfoFeatures bi sv

  checkAutogenModules ams bi

  -- PVP: we check for base and all other deps.
  let ds = mergeDependencies $ targetBuildDepends bi
  (ids, rds) <-
    partitionDeps
      ads
      [mkUnqualComponentName "base"]
      ds
  let ick = const (PackageDistInexcusable BaseNoUpperBounds)
      rck = PackageDistSuspiciousWarn . MissingUpperBounds cet
      leuck = PackageDistSuspiciousWarn . LEUpperBounds cet
      tzuck = PackageDistSuspiciousWarn . TrailingZeroUpperBounds cet
      gtlck = PackageDistSuspiciousWarn . GTLowerBounds cet
  checkPVP (checkDependencyVersionRange $ not . hasUpperBound) ick ids
  unless
    (isInternalTarget cet)
    (checkPVPs (checkDependencyVersionRange $ not . hasUpperBound) rck rds)
  unless
    (isInternalTarget cet)
    (checkPVPs (checkDependencyVersionRange hasLEUpperBound) leuck ds)
  unless
    (isInternalTarget cet)
    (checkPVPs (checkDependencyVersionRange hasTrailingZeroUpperBound) tzuck ds)
  unless
    (isInternalTarget cet)
    (checkPVPs (checkDependencyVersionRange hasGTLowerBound) gtlck ds)

  -- Custom fields well-formedness (ASCII).
  mapM_ checkCustomField (customFieldsBI bi)

  -- Content.
  mapM_ (checkLocalPathExist "extra-lib-dirs" . getSymbolicPath) (extraLibDirs bi)
  mapM_
    (checkLocalPathExist "extra-lib-dirs-static" . getSymbolicPath)
    (extraLibDirsStatic bi)
  mapM_
    (checkLocalPathExist "extra-framework-dirs" . getSymbolicPath)
    (extraFrameworkDirs bi)
  mapM_ (checkLocalPathExist "include-dirs" . getSymbolicPath) (includeDirs bi)
  mapM_
    (checkLocalPathExist "hs-source-dirs" . getSymbolicPath)
    (hsSourceDirs bi)

-- Well formedness of BI contents (no `Haskell2015`, no deprecated
-- extensions etc).
checkBuildInfoPathsContent :: Monad m => BuildInfo -> CheckM m ()
checkBuildInfoPathsContent bi = do
  mapM_ checkLang (allLanguages bi)
  mapM_ checkExt (allExtensions bi)
  mapM_ checkIntDep (targetBuildDepends bi)
  df <- asksCM ccDesugar
  -- This way we can use the same function for legacy&non exedeps.
  let ds = buildToolDepends bi ++ catMaybes (map df $ buildTools bi)
  mapM_ checkBTDep ds
  where
    checkLang :: Monad m => Language -> CheckM m ()
    checkLang (UnknownLanguage n) =
      tellP (PackageBuildWarning (UnknownLanguages [n]))
    checkLang _ = return ()

    checkExt :: Monad m => Extension -> CheckM m ()
    checkExt (UnknownExtension n)
      | n `elem` map prettyShow knownLanguages =
          tellP (PackageBuildWarning (LanguagesAsExtension [n]))
      | otherwise =
          tellP (PackageBuildWarning (UnknownExtensions [n]))
    checkExt n = do
      let dss = filter (\(a, _) -> a == n) deprecatedExtensions
      checkP
        (not . null $ dss)
        (PackageDistSuspicious $ DeprecatedExtensions dss)

    checkIntDep :: Monad m => Dependency -> CheckM m ()
    checkIntDep d@(Dependency name vrange _) = do
      mpn <-
        asksCM
          ( packageNameToUnqualComponentName
              . pkgName
              . pnPackageId
              . ccNames
          )
      lns <- asksCM (pnSubLibs . ccNames)
      pVer <- asksCM (pkgVersion . pnPackageId . ccNames)
      let allLibNs = mpn : lns
      when
        ( mpn == packageNameToUnqualComponentName name
            -- Make sure it is not a library with the
            -- same name from another package.
            && packageNameToUnqualComponentName name `elem` allLibNs
        )
        ( checkP
            (not $ pVer `withinRange` vrange)
            (PackageBuildImpossible $ ImpossibleInternalDep [d])
        )

    checkBTDep :: Monad m => ExeDependency -> CheckM m ()
    checkBTDep ed@(ExeDependency n name vrange) = do
      exns <- asksCM (pnExecs . ccNames)
      pVer <- asksCM (pkgVersion . pnPackageId . ccNames)
      pNam <- asksCM (pkgName . pnPackageId . ccNames)
      checkP
        ( n == pNam
            && name `notElem` exns -- internal
            -- not present
        )
        (PackageBuildImpossible $ MissingInternalExe [ed])
      when
        (name `elem` exns)
        ( checkP
            (not $ pVer `withinRange` vrange)
            (PackageBuildImpossible $ ImpossibleInternalExe [ed])
        )

-- Paths well-formedness check for BuildInfo.
checkBuildInfoPathsWellFormedness :: Monad m => BuildInfo -> CheckM m ()
checkBuildInfoPathsWellFormedness bi = do
  mapM_ (checkPath False "asm-sources" PathKindFile . getSymbolicPath) (asmSources bi)
  mapM_ (checkPath False "cmm-sources" PathKindFile . getSymbolicPath) (cmmSources bi)
  mapM_ (checkPath False "c-sources" PathKindFile . getSymbolicPath) (cSources bi)
  mapM_ (checkPath False "cxx-sources" PathKindFile . getSymbolicPath) (cxxSources bi)
  mapM_ (checkPath False "js-sources" PathKindFile . getSymbolicPath) (jsSources bi)
  mapM_
    (checkPath False "install-includes" PathKindFile . getSymbolicPath)
    (installIncludes bi)
  mapM_
    (checkPath False "hs-source-dirs" PathKindDirectory . getSymbolicPath)
    (hsSourceDirs bi)
  -- Possibly absolute paths.
  mapM_ (checkPath True "includes" PathKindFile . getSymbolicPath) (includes bi)
  mapM_
    (checkPath True "include-dirs" PathKindDirectory . getSymbolicPath)
    (includeDirs bi)
  mapM_
    (checkPath True "extra-lib-dirs" PathKindDirectory . getSymbolicPath)
    (extraLibDirs bi)
  mapM_
    (checkPath True "extra-lib-dirs-static" PathKindDirectory . getSymbolicPath)
    (extraLibDirsStatic bi)
  mapM_ checkOptionPath (perCompilerFlavorToList $ options bi)
  where
    checkOptionPath
      :: Monad m
      => (CompilerFlavor, [FilePath])
      -> CheckM m ()
    checkOptionPath (GHC, paths) =
      mapM_
        ( \path ->
            checkP
              (isInsideDist path)
              (PackageDistInexcusable $ DistPoint Nothing path)
        )
        paths
    checkOptionPath _ = return ()

-- Checks for features that can be present in BuildInfo only with certain
-- CabalSpecVersion.
checkBuildInfoFeatures
  :: Monad m
  => BuildInfo
  -> CabalSpecVersion
  -> CheckM m ()
checkBuildInfoFeatures bi sv = do
  -- Default language can be used only w/ spec ≥ 1.10
  checkSpecVer
    CabalSpecV1_10
    (isJust $ defaultLanguage bi)
    (PackageBuildWarning CVDefaultLanguage)
  -- CheckSpecVer sv.
  checkDefaultLanguage
  -- Check use of 'extra-framework-dirs' field.
  checkSpecVer
    CabalSpecV1_24
    (not . null $ extraFrameworkDirs bi)
    (PackageDistSuspiciousWarn CVExtraFrameworkDirs)
  -- Check use of default-extensions field don't need to do the
  -- equivalent check for other-extensions.
  checkSpecVer
    CabalSpecV1_10
    (not . null $ defaultExtensions bi)
    (PackageBuildWarning CVDefaultExtensions)
  -- Check use of extensions field
  checkP
    (sv >= CabalSpecV1_10 && (not . null $ oldExtensions bi))
    (PackageBuildWarning CVExtensionsDeprecated)

  -- asm-sources, cmm-sources and friends only w/ spec ≥ 1.10
  checkCVSources (map getSymbolicPath $ asmSources bi)
  checkCVSources (map getSymbolicPath $ cmmSources bi)
  checkCVSources (extraBundledLibs bi)
  checkCVSources (extraLibFlavours bi)

  -- extra-dynamic-library-flavours requires ≥ 3.0
  checkSpecVer
    CabalSpecV3_0
    (not . null $ extraDynLibFlavours bi)
    (PackageDistInexcusable $ CVExtraDynamic [extraDynLibFlavours bi])
  -- virtual-modules requires ≥ 2.2
  checkSpecVer CabalSpecV2_2 (not . null $ virtualModules bi) $
    (PackageDistInexcusable CVVirtualModules)
  -- Check use of thinning and renaming.
  checkSpecVer
    CabalSpecV2_0
    (not . null $ mixins bi)
    (PackageDistInexcusable CVMixins)

  checkBuildInfoExtensions bi
  where
    checkCVSources :: Monad m => [FilePath] -> CheckM m ()
    checkCVSources cvs =
      checkSpecVer
        CabalSpecV3_0
        (not . null $ cvs)
        (PackageDistInexcusable CVSources)

    checkDefaultLanguage :: Monad m => CheckM m ()
    checkDefaultLanguage = do
      -- < 1.10 has no `default-language` field.
      when
        (sv >= CabalSpecV1_10 && isNothing (defaultLanguage bi))
        -- < 3.4 mandatory, after just a suggestion.
        ( if sv < CabalSpecV3_4
            then tellP (PackageBuildWarning CVDefaultLanguageComponent)
            else tellP (PackageDistInexcusable CVDefaultLanguageComponentSoft)
        )

-- Tests for extensions usage which can break Cabal < 1.4.
checkBuildInfoExtensions :: Monad m => BuildInfo -> CheckM m ()
checkBuildInfoExtensions bi = do
  let exts = allExtensions bi
      extCabal1_2 = nub $ filter (`elem` compatExtensionsExtra) exts
      extCabal1_4 = nub $ filter (`notElem` compatExtensions) exts
  -- As of Cabal-1.4 we can add new extensions without worrying
  -- about breaking old versions of cabal.
  checkSpecVer
    CabalSpecV1_2
    (not . null $ extCabal1_2)
    ( PackageDistInexcusable $
        CVExtensions CabalSpecV1_2 extCabal1_2
    )
  checkSpecVer
    CabalSpecV1_4
    (not . null $ extCabal1_4)
    ( PackageDistInexcusable $
        CVExtensions CabalSpecV1_4 extCabal1_4
    )
  where
    -- The known extensions in Cabal-1.2.3
    compatExtensions :: [Extension]
    compatExtensions =
      map
        EnableExtension
        [ OverlappingInstances
        , UndecidableInstances
        , IncoherentInstances
        , RecursiveDo
        , ParallelListComp
        , MultiParamTypeClasses
        , FunctionalDependencies
        , Rank2Types
        , RankNTypes
        , PolymorphicComponents
        , ExistentialQuantification
        , ScopedTypeVariables
        , ImplicitParams
        , FlexibleContexts
        , FlexibleInstances
        , EmptyDataDecls
        , CPP
        , BangPatterns
        , TypeSynonymInstances
        , TemplateHaskell
        , ForeignFunctionInterface
        , Arrows
        , Generics
        , NamedFieldPuns
        , PatternGuards
        , GeneralizedNewtypeDeriving
        , ExtensibleRecords
        , RestrictedTypeSynonyms
        , HereDocuments
        ]
        ++ map
          DisableExtension
          [MonomorphismRestriction, ImplicitPrelude]
        ++ compatExtensionsExtra

    -- The extra known extensions in Cabal-1.2.3 vs Cabal-1.1.6
    -- (Cabal-1.1.6 came with ghc-6.6. Cabal-1.2 came with ghc-6.8)
    compatExtensionsExtra :: [Extension]
    compatExtensionsExtra =
      map
        EnableExtension
        [ KindSignatures
        , MagicHash
        , TypeFamilies
        , StandaloneDeriving
        , UnicodeSyntax
        , PatternSignatures
        , UnliftedFFITypes
        , LiberalTypeSynonyms
        , TypeOperators
        , RecordWildCards
        , RecordPuns
        , DisambiguateRecordFields
        , OverloadedStrings
        , GADTs
        , RelaxedPolyRec
        , ExtendedDefaultRules
        , UnboxedTuples
        , DeriveDataTypeable
        , ConstrainedClassMethods
        ]
        ++ map
          DisableExtension
          [MonoPatBinds]

-- Autogenerated modules (Paths_, PackageInfo_) checks. We could pass this
-- function something more specific than the whole BuildInfo, but it would be
-- a tuple of [ModuleName] lists, error prone.
checkAutogenModules
  :: Monad m
  => [ModuleName] -- Additional modules not present
  -- in BuildInfo (e.g. exposed library
  -- modules).
  -> BuildInfo
  -> CheckM m ()
checkAutogenModules ams bi = do
  pkgId <- asksCM (pnPackageId . ccNames)
  let
    -- It is an unfortunate reality that autogenPathsModuleName
    -- and autogenPackageInfoModuleName work on PackageDescription
    -- while not needing it all, but just the `package` bit.
    minimalPD = emptyPackageDescription{package = pkgId}
    autoPathsName = autogenPathsModuleName minimalPD
    autoInfoModuleName = autogenPackageInfoModuleName minimalPD

  -- Autogenerated module + some default extension build failure.
  autogenCheck autoPathsName CVAutogenPaths
  rebindableClashCheck autoPathsName RebindableClashPaths

  -- Paths_* module + some default extension build failure.
  autogenCheck autoInfoModuleName CVAutogenPackageInfo
  rebindableClashCheck autoInfoModuleName RebindableClashPackageInfo

  -- PackageInfo_* module + cabal-version < 3.12
  -- See Mikolaj’s comments on #9481 on why this has to be
  -- PackageBuildImpossible and not merely PackageDistInexcusable.
  checkSpecVer
    CabalSpecV3_12
    (elem autoInfoModuleName allModsForAuto)
    (PackageBuildImpossible CVAutogenPackageInfoGuard)
  where
    allModsForAuto :: [ModuleName]
    allModsForAuto = ams ++ otherModules bi

    autogenCheck
      :: Monad m
      => ModuleName
      -> CheckExplanation
      -> CheckM m ()
    autogenCheck name warning = do
      sv <- asksCM ccSpecVersion
      checkP
        ( sv >= CabalSpecV2_0
            && elem name allModsForAuto
            && notElem name (autogenModules bi)
        )
        (PackageDistInexcusable warning)

    rebindableClashCheck
      :: Monad m
      => ModuleName
      -> CheckExplanation
      -> CheckM m ()
    rebindableClashCheck name warning = do
      checkSpecVer
        CabalSpecV2_2
        ( ( name `elem` otherModules bi
              || name `elem` autogenModules bi
          )
            && checkExts
        )
        (PackageBuildImpossible warning)

    -- Do we have some peculiar extensions active which would interfere
    -- (cabal-version <2.2) with Paths_modules?
    checkExts :: Bool
    checkExts =
      let exts = defaultExtensions bi
       in rebind `elem` exts
            && (strings `elem` exts || lists `elem` exts)
      where
        rebind = EnableExtension RebindableSyntax
        strings = EnableExtension OverloadedStrings
        lists = EnableExtension OverloadedLists

checkLocalPathExist
  :: Monad m
  => String -- .cabal field where we found the error.
  -> FilePath
  -> CheckM m ()
checkLocalPathExist title dir =
  checkPkg
    ( \ops -> do
        dn <- not <$> doesDirectoryExist ops dir
        let rp = not (isAbsoluteOnAnyPlatform dir)
        return (rp && dn)
    )
    (PackageBuildWarning $ UnknownDirectory title dir)

-- PVP --

-- Sometimes we read (or end up with) “straddle” deps declarations
-- like this:
--
--     build-depends: base > 3, base < 4
--
-- `mergeDependencies` reduces that to base > 3 && < 4, _while_ maintaining
-- dependencies order in the list (better UX).
mergeDependencies :: [Dependency] -> [Dependency]
mergeDependencies [] = []
mergeDependencies l@(d : _) =
  let (sames, diffs) = partition ((== depName d) . depName) l
      merged =
        Dependency
          (depPkgName d)
          ( foldl intersectVersionRanges anyVersion $
              map depVerRange sames
          )
          (depLibraries d)
   in merged : mergeDependencies diffs
  where
    depName :: Dependency -> String
    depName wd = unPackageName . depPkgName $ wd

-- Is this an internal target? We do not perform PVP checks on those,
-- see https://github.com/haskell/cabal/pull/8361#issuecomment-1577547091
isInternalTarget :: CEType -> Bool
isInternalTarget (CETLibrary{}) = False
isInternalTarget (CETForeignLibrary{}) = False
isInternalTarget (CETExecutable{}) = False
isInternalTarget (CETTest{}) = True
isInternalTarget (CETBenchmark{}) = True
isInternalTarget (CETSetup{}) = False

-- ------------------------------------------------------------
-- Options
-- ------------------------------------------------------------

-- Target type for option checking.
data BITarget = BITLib | BITTestBench | BITOther
  deriving (Eq, Show)

cet2bit :: CEType -> BITarget
cet2bit (CETLibrary{}) = BITLib
cet2bit (CETForeignLibrary{}) = BITLib
cet2bit (CETExecutable{}) = BITOther
cet2bit (CETTest{}) = BITTestBench
cet2bit (CETBenchmark{}) = BITTestBench
cet2bit CETSetup = BITOther

-- General check on all options (ghc, C, C++, …) for common inaccuracies.
checkBuildInfoOptions :: Monad m => BITarget -> BuildInfo -> CheckM m ()
checkBuildInfoOptions t bi = do
  checkGHCOptions "ghc-options" t (hcOptions GHC bi)
  checkGHCOptions "ghc-prof-options" t (hcProfOptions GHC bi)
  checkGHCOptions "ghc-shared-options" t (hcSharedOptions GHC bi)
  let ldOpts = ldOptions bi
  checkCLikeOptions LangC "cc-options" (ccOptions bi) ldOpts
  checkCLikeOptions LangCPlusPlus "cxx-options" (cxxOptions bi) ldOpts
  checkCPPOptions (cppOptions bi)
  checkJSPOptions (jsppOptions bi)

-- | Checks GHC options for commonly misused or non-portable flags.
checkGHCOptions
  :: Monad m
  => CabalField -- .cabal field name where we found the error.
  -> BITarget -- Target type.
  -> [String] -- Options (alas in String form).
  -> CheckM m ()
checkGHCOptions title t opts = do
  checkGeneral
  case t of
    BITLib -> sequence_ [checkLib, checkNonTestBench]
    BITTestBench -> checkTestBench
    BITOther -> checkNonTestBench
  where
    checkFlags :: Monad m => [String] -> PackageCheck -> CheckM m ()
    checkFlags fs ck = checkP (any (`elem` fs) opts) ck

    checkFlagsP
      :: Monad m
      => (String -> Bool)
      -> (String -> PackageCheck)
      -> CheckM m ()
    checkFlagsP p ckc =
      case filter p opts of
        [] -> return ()
        (_ : _) -> tellP (ckc title)

    checkGeneral = do
      checkFlags
        ["-fasm"]
        (PackageDistInexcusable $ OptFasm title)
      checkFlags
        ["-fhpc"]
        (PackageDistInexcusable $ OptHpc title)
      checkFlags
        ["-prof"]
        (PackageBuildWarning $ OptProf title)
      pid <- asksCM (pnPackageId . ccNames)
      -- Scripts add the -o flag in the fake-package.cabal in order to have the
      -- executable name match the script name even when there are characters
      -- in the script name which are illegal to have as a target name.
      unless (pid == fakePackageId) $
        checkFlags
          ["-o"]
          (PackageBuildWarning $ OptO title)
      checkFlags
        ["-hide-package"]
        (PackageBuildWarning $ OptHide title)
      checkFlags
        ["--make"]
        (PackageBuildWarning $ OptMake title)
      checkFlags
        ["-O", "-O1"]
        (PackageDistInexcusable $ OptOOne title)
      checkFlags
        ["-O2"]
        (PackageDistSuspiciousWarn $ OptOTwo title)
      checkFlags
        ["-split-sections"]
        (PackageBuildWarning $ OptSplitSections title)
      checkFlags
        ["-split-objs"]
        (PackageBuildWarning $ OptSplitObjs title)
      checkFlags
        ["-optl-Wl,-s", "-optl-s"]
        (PackageDistInexcusable $ OptWls title)
      checkFlags
        ["-fglasgow-exts"]
        (PackageDistSuspicious $ OptExts title)
      let ghcNoRts = rmRtsOpts opts
      checkAlternatives
        title
        "default-extensions"
        [ (flag, prettyShow extension)
        | flag <- ghcNoRts
        , Just extension <- [ghcExtension flag]
        ]
      checkAlternatives
        title
        "default-extensions"
        [ (flag, extension)
        | flag@('-' : 'X' : extension) <- ghcNoRts
        ]
      checkAlternatives
        title
        "cpp-options"
        ( [(flag, flag) | flag@('-' : 'D' : _) <- ghcNoRts]
            ++ [(flag, flag) | flag@('-' : 'U' : _) <- ghcNoRts]
        )
      checkAlternatives
        title
        "jspp-options"
        ( [(flag, flag) | flag@('-' : 'D' : _) <- ghcNoRts]
            ++ [(flag, flag) | flag@('-' : 'U' : _) <- ghcNoRts]
        )
      checkAlternatives
        title
        "include-dirs"
        [(flag, dir) | flag@('-' : 'I' : dir) <- ghcNoRts]
      checkAlternatives
        title
        "extra-libraries"
        [(flag, lib) | flag@('-' : 'l' : lib) <- ghcNoRts]
      checkAlternatives
        title
        "extra-libraries-static"
        [(flag, lib) | flag@('-' : 'l' : lib) <- ghcNoRts]
      checkAlternatives
        title
        "extra-lib-dirs"
        [(flag, dir) | flag@('-' : 'L' : dir) <- ghcNoRts]
      checkAlternatives
        title
        "extra-lib-dirs-static"
        [(flag, dir) | flag@('-' : 'L' : dir) <- ghcNoRts]
      checkAlternatives
        title
        "frameworks"
        [ (flag, fmwk)
        | (flag@"-framework", fmwk) <-
            zip ghcNoRts (safeTail ghcNoRts)
        ]
      checkAlternatives
        title
        "extra-framework-dirs"
        [ (flag, dir)
        | (flag@"-framework-path", dir) <-
            zip ghcNoRts (safeTail ghcNoRts)
        ]
      -- Old `checkDevelopmentOnlyFlagsOptions` section
      checkFlags
        ["-Werror"]
        (PackageDistInexcusable $ WErrorUnneeded title)
      checkFlags
        ["-fdefer-type-errors"]
        (PackageDistInexcusable $ FDeferTypeErrorsUnneeded title)
      checkFlags
        [ "-fprof-auto"
        , "-fprof-auto-top"
        , "-fprof-auto-calls"
        , "-fprof-cafs"
        , "-fno-prof-count-entries"
        , "-auto-all"
        , "-auto"
        , "-caf-all"
        ]
        (PackageDistSuspicious $ ProfilingUnneeded title)
      checkFlagsP
        ( \opt ->
            "-d" `isPrefixOf` opt
              && opt /= "-dynamic"
        )
        (PackageDistInexcusable . DynamicUnneeded)
      checkFlagsP
        ( \opt -> case opt of
            "-j" -> True
            ('-' : 'j' : d : _) -> isDigit d
            _ -> False
        )
        (PackageDistInexcusable . JUnneeded)

    checkLib = do
      checkP
        ("-rtsopts" `elem` opts)
        (PackageBuildWarning $ OptRts title)
      checkP
        (any (\opt -> "-with-rtsopts" `isPrefixOf` opt) opts)
        (PackageBuildWarning $ OptWithRts title)

    checkTestBench = do
      checkFlags
        ["-O0", "-Onot"]
        (PackageDistSuspiciousWarn $ OptONot title)

    checkNonTestBench = do
      checkFlags
        ["-O0", "-Onot"]
        (PackageDistSuspicious $ OptONot title)

    ghcExtension ('-' : 'f' : name) = case name of
      "allow-overlapping-instances" -> enable OverlappingInstances
      "no-allow-overlapping-instances" -> disable OverlappingInstances
      "th" -> enable TemplateHaskell
      "no-th" -> disable TemplateHaskell
      "ffi" -> enable ForeignFunctionInterface
      "no-ffi" -> disable ForeignFunctionInterface
      "fi" -> enable ForeignFunctionInterface
      "no-fi" -> disable ForeignFunctionInterface
      "monomorphism-restriction" -> enable MonomorphismRestriction
      "no-monomorphism-restriction" -> disable MonomorphismRestriction
      "mono-pat-binds" -> enable MonoPatBinds
      "no-mono-pat-binds" -> disable MonoPatBinds
      "allow-undecidable-instances" -> enable UndecidableInstances
      "no-allow-undecidable-instances" -> disable UndecidableInstances
      "allow-incoherent-instances" -> enable IncoherentInstances
      "no-allow-incoherent-instances" -> disable IncoherentInstances
      "arrows" -> enable Arrows
      "no-arrows" -> disable Arrows
      "generics" -> enable Generics
      "no-generics" -> disable Generics
      "implicit-prelude" -> enable ImplicitPrelude
      "no-implicit-prelude" -> disable ImplicitPrelude
      "implicit-params" -> enable ImplicitParams
      "no-implicit-params" -> disable ImplicitParams
      "bang-patterns" -> enable BangPatterns
      "no-bang-patterns" -> disable BangPatterns
      "scoped-type-variables" -> enable ScopedTypeVariables
      "no-scoped-type-variables" -> disable ScopedTypeVariables
      "extended-default-rules" -> enable ExtendedDefaultRules
      "no-extended-default-rules" -> disable ExtendedDefaultRules
      _ -> Nothing
    ghcExtension "-cpp" = enable CPP
    ghcExtension _ = Nothing

    enable e = Just (EnableExtension e)
    disable e = Just (DisableExtension e)

    rmRtsOpts :: [String] -> [String]
    rmRtsOpts ("-with-rtsopts" : _ : xs) = rmRtsOpts xs
    rmRtsOpts (x : xs) = x : rmRtsOpts xs
    rmRtsOpts [] = []

checkCLikeOptions
  :: Monad m
  => WarnLang -- Language we are warning about (C or C++).
  -> CabalField -- Field where we found the error.
  -> [String] -- Options in string form.
  -> [String] -- Link options in String form.
  -> CheckM m ()
checkCLikeOptions label prefix opts ldOpts = do
  checkAlternatives
    prefix
    "include-dirs"
    [(flag, dir) | flag@('-' : 'I' : dir) <- opts]
  checkAlternatives
    prefix
    "extra-libraries"
    [(flag, lib) | flag@('-' : 'l' : lib) <- opts]
  checkAlternatives
    prefix
    "extra-lib-dirs"
    [(flag, dir) | flag@('-' : 'L' : dir) <- opts]

  checkAlternatives
    "ld-options"
    "extra-libraries"
    [(flag, lib) | flag@('-' : 'l' : lib) <- ldOpts]
  checkAlternatives
    "ld-options"
    "extra-lib-dirs"
    [(flag, dir) | flag@('-' : 'L' : dir) <- ldOpts]

  checkP
    (any (`elem` ["-O", "-Os", "-O0", "-O1", "-O2", "-O3"]) opts)
    (PackageDistSuspicious $ COptONumber prefix label)

checkAlternatives
  :: Monad m
  => CabalField -- Wrong field.
  -> CabalField -- Appropriate field.
  -> [(String, String)] -- List of good and bad flags.
  -> CheckM m ()
checkAlternatives badField goodField flags = do
  let (badFlags, _) = unzip flags
  checkP
    (not $ null badFlags)
    (PackageBuildWarning $ OptAlternatives badField goodField flags)

checkCPPOptions
  :: Monad m
  => [String] -- Options in String form.
  -> CheckM m ()
checkCPPOptions opts = do
  checkAlternatives
    "cpp-options"
    "include-dirs"
    [(flag, dir) | flag@('-' : 'I' : dir) <- opts]
  mapM_
    ( \opt ->
        checkP
          (not $ any (`isPrefixOf` opt) ["-D", "-U", "-I"])
          (PackageBuildWarning (COptCPP opt))
    )
    opts

checkJSPOptions
  :: Monad m
  => [String] -- Options in String form.
  -> CheckM m ()
checkJSPOptions opts = do
  checkAlternatives
    "jspp-options"
    "include-dirs"
    [(flag, dir) | flag@('-' : 'I' : dir) <- opts]
  mapM_
    ( \opt ->
        checkP
          (not $ any (`isPrefixOf` opt) ["-D", "-U", "-I"])
          (PackageBuildWarning (OptJSPP opt))
    )
    opts
