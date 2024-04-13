{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Distribution.PackageDescription.Check.Warning
-- Copyright   :  Francesco Ariis 2022
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Warning types, messages, severity and associated functions.
module Distribution.PackageDescription.Check.Warning
  ( -- * Types and constructors
    PackageCheck (..)
  , CheckExplanation (..)
  , CheckExplanationID
  , CheckExplanationIDString
  , CEType (..)
  , WarnLang (..)

    -- * Operations
  , ppPackageCheck
  , ppCheckExplanationId
  , isHackageDistError
  , extractCheckExplantion
  , filterPackageChecksById
  , filterPackageChecksByIdString
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion, showCabalSpecVersion)
import Distribution.License (License, knownLicenses)
import Distribution.ModuleName (ModuleName)
import Distribution.Parsec.Warning (PWarning, showPWarning)
import Distribution.Pretty (prettyShow)
import Distribution.Types.BenchmarkType (BenchmarkType, knownBenchmarkTypes)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency)
import Distribution.Types.Flag (FlagName, unFlagName)
import Distribution.Types.LibraryName (LibraryName (..), showLibraryName)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.TestType (TestType, knownTestTypes)
import Distribution.Types.UnqualComponentName
import Distribution.Types.Version (Version)
import Distribution.Utils.Path
import Language.Haskell.Extension (Extension)

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Set as Set

-- ------------------------------------------------------------
-- Check types and explanations
-- ------------------------------------------------------------

-- | Results of some kind of failed package check.
--
-- There are a range of severities, from merely dubious to totally insane.
-- All of them come with a human readable explanation. In future we may augment
-- them with more machine readable explanations, for example to help an IDE
-- suggest automatic corrections.
data PackageCheck
  = -- | This package description is no good. There's no way it's going to
    -- build sensibly. This should give an error at configure time.
    PackageBuildImpossible {explanation :: CheckExplanation}
  | -- | A problem that is likely to affect building the package, or an
    -- issue that we'd like every package author to be aware of, even if
    -- the package is never distributed.
    PackageBuildWarning {explanation :: CheckExplanation}
  | -- | An issue that might not be a problem for the package author but
    -- might be annoying or detrimental when the package is distributed to
    -- users. We should encourage distributed packages to be free from these
    -- issues, but occasionally there are justifiable reasons so we cannot
    -- ban them entirely.
    PackageDistSuspicious {explanation :: CheckExplanation}
  | -- | Like PackageDistSuspicious but will only display warnings
    -- rather than causing abnormal exit when you run 'cabal check'.
    PackageDistSuspiciousWarn {explanation :: CheckExplanation}
  | -- | An issue that is OK in the author's environment but is almost
    -- certain to be a portability problem for other environments. We can
    -- quite legitimately refuse to publicly distribute packages with these
    -- problems.
    PackageDistInexcusable {explanation :: CheckExplanation}
  deriving (Eq, Ord)

-- | Pretty printing 'PackageCheck'.
ppPackageCheck :: PackageCheck -> String
ppPackageCheck e =
  let ex = explanation e
   in "["
        ++ (ppCheckExplanationId . checkExplanationId) ex
        ++ "] "
        ++ ppExplanation ex

-- | Broken 'Show' instance (not bijective with Read), alas external packages
-- depend on it.
instance Show PackageCheck where
  show notice = ppPackageCheck notice

-- | Would Hackage refuse a package because of this error?
isHackageDistError :: PackageCheck -> Bool
isHackageDistError = \case
  (PackageBuildImpossible{}) -> True
  (PackageBuildWarning{}) -> True
  (PackageDistInexcusable{}) -> True
  (PackageDistSuspicious{}) -> False
  (PackageDistSuspiciousWarn{}) -> False

-- | Filter Package Check by CheckExplanationID.
filterPackageChecksById
  :: [PackageCheck]
  -- ^ Original checks.
  -> [CheckExplanationID]
  -- ^ IDs to omit.
  -> [PackageCheck]
filterPackageChecksById cs is = filter ff cs
  where
    ff :: PackageCheck -> Bool
    ff c =
      flip notElem is
        . checkExplanationId
        . extractCheckExplantion
        $ c

-- | Filter Package Check by Check explanation /string/.
filterPackageChecksByIdString
  :: [PackageCheck]
  -- ^ Original checks.
  -> [CheckExplanationIDString]
  -- ^ IDs to omit, in @String@ format.
  -> ([PackageCheck], [CheckExplanationIDString])
-- Filtered checks plus unrecognised id strings.
filterPackageChecksByIdString cs ss =
  let (es, is) = Either.partitionEithers $ map readExplanationID ss
   in (filterPackageChecksById cs is, es)

-- | Explanations of 'PackageCheck`'s errors/warnings.
data CheckExplanation
  = ParseWarning FilePath PWarning
  | NoNameField
  | NoVersionField
  | NoTarget
  | UnnamedInternal
  | DuplicateSections [UnqualComponentName]
  | IllegalLibraryName PackageName
  | NoModulesExposed LibraryName
  | SignaturesCabal2
  | AutogenNotExposed
  | AutogenIncludesNotIncluded
  | NoMainIs UnqualComponentName
  | NoHsLhsMain
  | MainCCabal1_18
  | AutogenNoOther CEType
  | AutogenIncludesNotIncludedExe
  | TestsuiteTypeNotKnown TestType
  | TestsuiteNotSupported TestType
  | BenchmarkTypeNotKnown BenchmarkType
  | BenchmarkNotSupported BenchmarkType
  | NoHsLhsMainBench
  | InvalidNameWin PackageName
  | ZPrefix
  | NoBuildType
  | NoCustomSetup
  | UnknownCompilers [String]
  | UnknownLanguages [String]
  | UnknownExtensions [String]
  | LanguagesAsExtension [String]
  | DeprecatedExtensions [(Extension, Maybe Extension)]
  | MissingFieldCategory
  | MissingFieldMaintainer
  | MissingFieldSynopsis
  | MissingFieldDescription
  | MissingFieldSynOrDesc
  | SynopsisTooLong
  | ShortDesc
  | InvalidTestWith [Dependency]
  | ImpossibleInternalDep [Dependency]
  | ImpossibleInternalExe [ExeDependency]
  | MissingInternalExe [ExeDependency]
  | NONELicense
  | NoLicense
  | AllRightsReservedLicense
  | LicenseMessParse License
  | UnrecognisedLicense String
  | UncommonBSD4
  | UnknownLicenseVersion License [Version]
  | NoLicenseFile
  | UnrecognisedSourceRepo String
  | MissingType
  | MissingLocation
  | MissingModule
  | MissingTag
  | SubdirRelPath
  | SubdirGoodRelPath String
  | OptFasm String
  | OptHpc String
  | OptProf String
  | OptO String
  | OptHide String
  | OptMake String
  | OptONot String
  | OptOOne String
  | OptOTwo String
  | OptSplitSections String
  | OptSplitObjs String
  | OptWls String
  | OptExts String
  | OptRts String
  | OptWithRts String
  | COptONumber String WarnLang
  | COptCPP String
  | OptAlternatives String String [(String, String)]
  | RelativeOutside String FilePath
  | AbsolutePath String FilePath
  | BadRelativePath String FilePath String
  | DistPoint (Maybe String) FilePath
  | GlobSyntaxError String String
  | RecursiveGlobInRoot String FilePath
  | InvalidOnWin [FilePath]
  | FilePathTooLong FilePath
  | FilePathNameTooLong FilePath
  | FilePathSplitTooLong FilePath
  | FilePathEmpty
  | CVTestSuite
  | CVDefaultLanguage
  | CVDefaultLanguageComponent
  | CVDefaultLanguageComponentSoft
  | CVExtraDocFiles
  | CVMultiLib
  | CVReexported
  | CVMixins
  | CVExtraFrameworkDirs
  | CVDefaultExtensions
  | CVExtensionsDeprecated
  | CVSources
  | CVExtraDynamic [[String]]
  | CVVirtualModules
  | CVSourceRepository
  | CVExtensions CabalSpecVersion [Extension]
  | CVCustomSetup
  | CVExpliticDepsCustomSetup
  | CVAutogenPaths
  | CVAutogenPackageInfo
  | CVAutogenPackageInfoGuard
  | GlobNoMatch String String
  | GlobExactMatch String String FilePath
  | GlobNoDir String String FilePath
  | UnknownOS [String]
  | UnknownArch [String]
  | UnknownCompiler [String]
  | BaseNoUpperBounds
  | MissingUpperBounds CEType [String]
  | SuspiciousFlagName [String]
  | DeclaredUsedFlags (Set.Set FlagName) (Set.Set FlagName)
  | NonASCIICustomField [String]
  | RebindableClashPaths
  | RebindableClashPackageInfo
  | WErrorUnneeded String
  | JUnneeded String
  | FDeferTypeErrorsUnneeded String
  | DynamicUnneeded String
  | ProfilingUnneeded String
  | UpperBoundSetup String
  | DuplicateModule String [ModuleName]
  | PotentialDupModule String [ModuleName]
  | BOMStart FilePath
  | NotPackageName FilePath String
  | NoDesc
  | MultiDesc [String]
  | UnknownFile String (RelativePath Pkg File)
  | MissingSetupFile
  | MissingConfigureScript
  | UnknownDirectory String FilePath
  | MissingSourceControl
  | MissingExpectedDocFiles Bool [FilePath]
  | WrongFieldForExpectedDocFiles Bool String [FilePath]
  deriving (Eq, Ord, Show)

-- TODO Some checks have a constructor in list form
--      (e.g. `SomeWarn [n]`), CheckM m () correctly catches warnings in
--      different stanzas in different checks (so it is not one soup).
--
--      Ideally [SomeWar [a], SomeWar [b]] would be translated into
--      SomeWar [a,b] in the few cases where it is appropriate for UX
--      and left separated otherwise.
--      To achieve this the Writer part of CheckM could be modified
--      to be a ad hoc monoid.

-- Convenience.
extractCheckExplantion :: PackageCheck -> CheckExplanation
extractCheckExplantion (PackageBuildImpossible e) = e
extractCheckExplantion (PackageBuildWarning e) = e
extractCheckExplantion (PackageDistSuspicious e) = e
extractCheckExplantion (PackageDistSuspiciousWarn e) = e
extractCheckExplantion (PackageDistInexcusable e) = e

-- | Identifier for the speficic 'CheckExplanation'. This ensures `--ignore`
-- can output a warning on unrecognised values.
-- ☞ N.B.: should be kept in sync with 'CheckExplanation'.
data CheckExplanationID
  = CIParseWarning
  | CINoNameField
  | CINoVersionField
  | CINoTarget
  | CIUnnamedInternal
  | CIDuplicateSections
  | CIIllegalLibraryName
  | CINoModulesExposed
  | CISignaturesCabal2
  | CIAutogenNotExposed
  | CIAutogenIncludesNotIncluded
  | CINoMainIs
  | CINoHsLhsMain
  | CIMainCCabal1_18
  | CIAutogenNoOther
  | CIAutogenIncludesNotIncludedExe
  | CITestsuiteTypeNotKnown
  | CITestsuiteNotSupported
  | CIBenchmarkTypeNotKnown
  | CIBenchmarkNotSupported
  | CINoHsLhsMainBench
  | CIInvalidNameWin
  | CIZPrefix
  | CINoBuildType
  | CINoCustomSetup
  | CIUnknownCompilers
  | CIUnknownLanguages
  | CIUnknownExtensions
  | CILanguagesAsExtension
  | CIDeprecatedExtensions
  | CIMissingFieldCategory
  | CIMissingFieldMaintainer
  | CIMissingFieldSynopsis
  | CIMissingFieldDescription
  | CIMissingFieldSynOrDesc
  | CISynopsisTooLong
  | CIShortDesc
  | CIInvalidTestWith
  | CIImpossibleInternalDep
  | CIImpossibleInternalExe
  | CIMissingInternalExe
  | CINONELicense
  | CINoLicense
  | CIAllRightsReservedLicense
  | CILicenseMessParse
  | CIUnrecognisedLicense
  | CIUncommonBSD4
  | CIUnknownLicenseVersion
  | CINoLicenseFile
  | CIUnrecognisedSourceRepo
  | CIMissingType
  | CIMissingLocation
  | CIMissingModule
  | CIMissingTag
  | CISubdirRelPath
  | CISubdirGoodRelPath
  | CIOptFasm
  | CIOptHpc
  | CIOptProf
  | CIOptO
  | CIOptHide
  | CIOptMake
  | CIOptONot
  | CIOptOOne
  | CIOptOTwo
  | CIOptSplitSections
  | CIOptSplitObjs
  | CIOptWls
  | CIOptExts
  | CIOptRts
  | CIOptWithRts
  | CICOptONumber
  | CICOptCPP
  | CIOptAlternatives
  | CIRelativeOutside
  | CIAbsolutePath
  | CIBadRelativePath
  | CIDistPoint
  | CIGlobSyntaxError
  | CIRecursiveGlobInRoot
  | CIInvalidOnWin
  | CIFilePathTooLong
  | CIFilePathNameTooLong
  | CIFilePathSplitTooLong
  | CIFilePathEmpty
  | CICVTestSuite
  | CICVDefaultLanguage
  | CICVDefaultLanguageComponent
  | CICVDefaultLanguageComponentSoft
  | CICVExtraDocFiles
  | CICVMultiLib
  | CICVReexported
  | CICVMixins
  | CICVExtraFrameworkDirs
  | CICVDefaultExtensions
  | CICVExtensionsDeprecated
  | CICVSources
  | CICVExtraDynamic
  | CICVVirtualModules
  | CICVSourceRepository
  | CICVExtensions
  | CICVCustomSetup
  | CICVExpliticDepsCustomSetup
  | CICVAutogenPaths
  | CICVAutogenPackageInfo
  | CICVAutogenPackageInfoGuard
  | CIGlobNoMatch
  | CIGlobExactMatch
  | CIGlobNoDir
  | CIUnknownOS
  | CIUnknownArch
  | CIUnknownCompiler
  | CIBaseNoUpperBounds
  | CIMissingUpperBounds
  | CISuspiciousFlagName
  | CIDeclaredUsedFlags
  | CINonASCIICustomField
  | CIRebindableClashPaths
  | CIRebindableClashPackageInfo
  | CIWErrorUnneeded
  | CIJUnneeded
  | CIFDeferTypeErrorsUnneeded
  | CIDynamicUnneeded
  | CIProfilingUnneeded
  | CIUpperBoundSetup
  | CIDuplicateModule
  | CIPotentialDupModule
  | CIBOMStart
  | CINotPackageName
  | CINoDesc
  | CIMultiDesc
  | CIUnknownFile
  | CIMissingSetupFile
  | CIMissingConfigureScript
  | CIUnknownDirectory
  | CIMissingSourceControl
  | CIMissingExpectedDocFiles
  | CIWrongFieldForExpectedDocFiles
  deriving (Eq, Ord, Show, Enum, Bounded)

checkExplanationId :: CheckExplanation -> CheckExplanationID
checkExplanationId (ParseWarning{}) = CIParseWarning
checkExplanationId (NoNameField{}) = CINoNameField
checkExplanationId (NoVersionField{}) = CINoVersionField
checkExplanationId (NoTarget{}) = CINoTarget
checkExplanationId (UnnamedInternal{}) = CIUnnamedInternal
checkExplanationId (DuplicateSections{}) = CIDuplicateSections
checkExplanationId (IllegalLibraryName{}) = CIIllegalLibraryName
checkExplanationId (NoModulesExposed{}) = CINoModulesExposed
checkExplanationId (SignaturesCabal2{}) = CISignaturesCabal2
checkExplanationId (AutogenNotExposed{}) = CIAutogenNotExposed
checkExplanationId (AutogenIncludesNotIncluded{}) = CIAutogenIncludesNotIncluded
checkExplanationId (NoMainIs{}) = CINoMainIs
checkExplanationId (NoHsLhsMain{}) = CINoHsLhsMain
checkExplanationId (MainCCabal1_18{}) = CIMainCCabal1_18
checkExplanationId (AutogenNoOther{}) = CIAutogenNoOther
checkExplanationId (AutogenIncludesNotIncludedExe{}) = CIAutogenIncludesNotIncludedExe
checkExplanationId (TestsuiteTypeNotKnown{}) = CITestsuiteTypeNotKnown
checkExplanationId (TestsuiteNotSupported{}) = CITestsuiteNotSupported
checkExplanationId (BenchmarkTypeNotKnown{}) = CIBenchmarkTypeNotKnown
checkExplanationId (BenchmarkNotSupported{}) = CIBenchmarkNotSupported
checkExplanationId (NoHsLhsMainBench{}) = CINoHsLhsMainBench
checkExplanationId (InvalidNameWin{}) = CIInvalidNameWin
checkExplanationId (ZPrefix{}) = CIZPrefix
checkExplanationId (NoBuildType{}) = CINoBuildType
checkExplanationId (NoCustomSetup{}) = CINoCustomSetup
checkExplanationId (UnknownCompilers{}) = CIUnknownCompilers
checkExplanationId (UnknownLanguages{}) = CIUnknownLanguages
checkExplanationId (UnknownExtensions{}) = CIUnknownExtensions
checkExplanationId (LanguagesAsExtension{}) = CILanguagesAsExtension
checkExplanationId (DeprecatedExtensions{}) = CIDeprecatedExtensions
checkExplanationId (MissingFieldCategory{}) = CIMissingFieldCategory
checkExplanationId (MissingFieldMaintainer{}) = CIMissingFieldMaintainer
checkExplanationId (MissingFieldSynopsis{}) = CIMissingFieldSynopsis
checkExplanationId (MissingFieldDescription{}) = CIMissingFieldDescription
checkExplanationId (MissingFieldSynOrDesc{}) = CIMissingFieldSynOrDesc
checkExplanationId (SynopsisTooLong{}) = CISynopsisTooLong
checkExplanationId (ShortDesc{}) = CIShortDesc
checkExplanationId (InvalidTestWith{}) = CIInvalidTestWith
checkExplanationId (ImpossibleInternalDep{}) = CIImpossibleInternalDep
checkExplanationId (ImpossibleInternalExe{}) = CIImpossibleInternalExe
checkExplanationId (MissingInternalExe{}) = CIMissingInternalExe
checkExplanationId (NONELicense{}) = CINONELicense
checkExplanationId (NoLicense{}) = CINoLicense
checkExplanationId (AllRightsReservedLicense{}) = CIAllRightsReservedLicense
checkExplanationId (LicenseMessParse{}) = CILicenseMessParse
checkExplanationId (UnrecognisedLicense{}) = CIUnrecognisedLicense
checkExplanationId (UncommonBSD4{}) = CIUncommonBSD4
checkExplanationId (UnknownLicenseVersion{}) = CIUnknownLicenseVersion
checkExplanationId (NoLicenseFile{}) = CINoLicenseFile
checkExplanationId (UnrecognisedSourceRepo{}) = CIUnrecognisedSourceRepo
checkExplanationId (MissingType{}) = CIMissingType
checkExplanationId (MissingLocation{}) = CIMissingLocation
checkExplanationId (MissingModule{}) = CIMissingModule
checkExplanationId (MissingTag{}) = CIMissingTag
checkExplanationId (SubdirRelPath{}) = CISubdirRelPath
checkExplanationId (SubdirGoodRelPath{}) = CISubdirGoodRelPath
checkExplanationId (OptFasm{}) = CIOptFasm
checkExplanationId (OptHpc{}) = CIOptHpc
checkExplanationId (OptProf{}) = CIOptProf
checkExplanationId (OptO{}) = CIOptO
checkExplanationId (OptHide{}) = CIOptHide
checkExplanationId (OptMake{}) = CIOptMake
checkExplanationId (OptONot{}) = CIOptONot
checkExplanationId (OptOOne{}) = CIOptOOne
checkExplanationId (OptOTwo{}) = CIOptOTwo
checkExplanationId (OptSplitSections{}) = CIOptSplitSections
checkExplanationId (OptSplitObjs{}) = CIOptSplitObjs
checkExplanationId (OptWls{}) = CIOptWls
checkExplanationId (OptExts{}) = CIOptExts
checkExplanationId (OptRts{}) = CIOptRts
checkExplanationId (OptWithRts{}) = CIOptWithRts
checkExplanationId (COptONumber{}) = CICOptONumber
checkExplanationId (COptCPP{}) = CICOptCPP
checkExplanationId (OptAlternatives{}) = CIOptAlternatives
checkExplanationId (RelativeOutside{}) = CIRelativeOutside
checkExplanationId (AbsolutePath{}) = CIAbsolutePath
checkExplanationId (BadRelativePath{}) = CIBadRelativePath
checkExplanationId (DistPoint{}) = CIDistPoint
checkExplanationId (GlobSyntaxError{}) = CIGlobSyntaxError
checkExplanationId (RecursiveGlobInRoot{}) = CIRecursiveGlobInRoot
checkExplanationId (InvalidOnWin{}) = CIInvalidOnWin
checkExplanationId (FilePathTooLong{}) = CIFilePathTooLong
checkExplanationId (FilePathNameTooLong{}) = CIFilePathNameTooLong
checkExplanationId (FilePathSplitTooLong{}) = CIFilePathSplitTooLong
checkExplanationId (FilePathEmpty{}) = CIFilePathEmpty
checkExplanationId (CVTestSuite{}) = CICVTestSuite
checkExplanationId (CVDefaultLanguage{}) = CICVDefaultLanguage
checkExplanationId (CVDefaultLanguageComponent{}) = CICVDefaultLanguageComponent
checkExplanationId (CVDefaultLanguageComponentSoft{}) = CICVDefaultLanguageComponentSoft
checkExplanationId (CVExtraDocFiles{}) = CICVExtraDocFiles
checkExplanationId (CVMultiLib{}) = CICVMultiLib
checkExplanationId (CVReexported{}) = CICVReexported
checkExplanationId (CVMixins{}) = CICVMixins
checkExplanationId (CVExtraFrameworkDirs{}) = CICVExtraFrameworkDirs
checkExplanationId (CVDefaultExtensions{}) = CICVDefaultExtensions
checkExplanationId (CVExtensionsDeprecated{}) = CICVExtensionsDeprecated
checkExplanationId (CVSources{}) = CICVSources
checkExplanationId (CVExtraDynamic{}) = CICVExtraDynamic
checkExplanationId (CVVirtualModules{}) = CICVVirtualModules
checkExplanationId (CVSourceRepository{}) = CICVSourceRepository
checkExplanationId (CVExtensions{}) = CICVExtensions
checkExplanationId (CVCustomSetup{}) = CICVCustomSetup
checkExplanationId (CVExpliticDepsCustomSetup{}) = CICVExpliticDepsCustomSetup
checkExplanationId (CVAutogenPaths{}) = CICVAutogenPaths
checkExplanationId (CVAutogenPackageInfo{}) = CICVAutogenPackageInfo
checkExplanationId (CVAutogenPackageInfoGuard{}) = CICVAutogenPackageInfoGuard
checkExplanationId (GlobNoMatch{}) = CIGlobNoMatch
checkExplanationId (GlobExactMatch{}) = CIGlobExactMatch
checkExplanationId (GlobNoDir{}) = CIGlobNoDir
checkExplanationId (UnknownOS{}) = CIUnknownOS
checkExplanationId (UnknownArch{}) = CIUnknownArch
checkExplanationId (UnknownCompiler{}) = CIUnknownCompiler
checkExplanationId (BaseNoUpperBounds{}) = CIBaseNoUpperBounds
checkExplanationId (MissingUpperBounds{}) = CIMissingUpperBounds
checkExplanationId (SuspiciousFlagName{}) = CISuspiciousFlagName
checkExplanationId (DeclaredUsedFlags{}) = CIDeclaredUsedFlags
checkExplanationId (NonASCIICustomField{}) = CINonASCIICustomField
checkExplanationId (RebindableClashPaths{}) = CIRebindableClashPaths
checkExplanationId (RebindableClashPackageInfo{}) = CIRebindableClashPackageInfo
checkExplanationId (WErrorUnneeded{}) = CIWErrorUnneeded
checkExplanationId (JUnneeded{}) = CIJUnneeded
checkExplanationId (FDeferTypeErrorsUnneeded{}) = CIFDeferTypeErrorsUnneeded
checkExplanationId (DynamicUnneeded{}) = CIDynamicUnneeded
checkExplanationId (ProfilingUnneeded{}) = CIProfilingUnneeded
checkExplanationId (UpperBoundSetup{}) = CIUpperBoundSetup
checkExplanationId (DuplicateModule{}) = CIDuplicateModule
checkExplanationId (PotentialDupModule{}) = CIPotentialDupModule
checkExplanationId (BOMStart{}) = CIBOMStart
checkExplanationId (NotPackageName{}) = CINotPackageName
checkExplanationId (NoDesc{}) = CINoDesc
checkExplanationId (MultiDesc{}) = CIMultiDesc
checkExplanationId (UnknownFile{}) = CIUnknownFile
checkExplanationId (MissingSetupFile{}) = CIMissingSetupFile
checkExplanationId (MissingConfigureScript{}) = CIMissingConfigureScript
checkExplanationId (UnknownDirectory{}) = CIUnknownDirectory
checkExplanationId (MissingSourceControl{}) = CIMissingSourceControl
checkExplanationId (MissingExpectedDocFiles{}) = CIMissingExpectedDocFiles
checkExplanationId (WrongFieldForExpectedDocFiles{}) = CIWrongFieldForExpectedDocFiles

type CheckExplanationIDString = String

-- A one-word identifier for each CheckExplanation
--
-- ☞ N.B: if you modify anything here, remeber to change the documentation
-- in @doc/cabal-commands.rst@!
ppCheckExplanationId :: CheckExplanationID -> CheckExplanationIDString
ppCheckExplanationId CIParseWarning = "parser-warning"
ppCheckExplanationId CINoNameField = "no-name-field"
ppCheckExplanationId CINoVersionField = "no-version-field"
ppCheckExplanationId CINoTarget = "no-target"
ppCheckExplanationId CIUnnamedInternal = "unnamed-internal-library"
ppCheckExplanationId CIDuplicateSections = "duplicate-sections"
ppCheckExplanationId CIIllegalLibraryName = "illegal-library-name"
ppCheckExplanationId CINoModulesExposed = "no-modules-exposed"
ppCheckExplanationId CISignaturesCabal2 = "signatures"
ppCheckExplanationId CIAutogenNotExposed = "autogen-not-exposed"
ppCheckExplanationId CIAutogenIncludesNotIncluded = "autogen-not-included"
ppCheckExplanationId CINoMainIs = "no-main-is"
ppCheckExplanationId CINoHsLhsMain = "unknown-extension-main"
ppCheckExplanationId CIMainCCabal1_18 = "c-like-main"
ppCheckExplanationId CIAutogenNoOther = "autogen-other-modules"
ppCheckExplanationId CIAutogenIncludesNotIncludedExe = "autogen-exe"
ppCheckExplanationId CITestsuiteTypeNotKnown = "unknown-testsuite-type"
ppCheckExplanationId CITestsuiteNotSupported = "unsupported-testsuite"
ppCheckExplanationId CIBenchmarkTypeNotKnown = "unknown-bench"
ppCheckExplanationId CIBenchmarkNotSupported = "unsupported-bench"
ppCheckExplanationId CINoHsLhsMainBench = "bench-unknown-extension"
ppCheckExplanationId CIInvalidNameWin = "invalid-name-win"
ppCheckExplanationId CIZPrefix = "reserved-z-prefix"
ppCheckExplanationId CINoBuildType = "no-build-type"
ppCheckExplanationId CINoCustomSetup = "undeclared-custom-setup"
ppCheckExplanationId CIUnknownCompilers = "unknown-compiler-tested"
ppCheckExplanationId CIUnknownLanguages = "unknown-languages"
ppCheckExplanationId CIUnknownExtensions = "unknown-extension"
ppCheckExplanationId CILanguagesAsExtension = "languages-as-extensions"
ppCheckExplanationId CIDeprecatedExtensions = "deprecated-extensions"
ppCheckExplanationId CIMissingFieldCategory = "no-category"
ppCheckExplanationId CIMissingFieldMaintainer = "no-maintainer"
ppCheckExplanationId CIMissingFieldSynopsis = "no-synopsis"
ppCheckExplanationId CIMissingFieldDescription = "no-description"
ppCheckExplanationId CIMissingFieldSynOrDesc = "no-syn-desc"
ppCheckExplanationId CISynopsisTooLong = "long-synopsis"
ppCheckExplanationId CIShortDesc = "short-description"
ppCheckExplanationId CIInvalidTestWith = "invalid-range-tested"
ppCheckExplanationId CIImpossibleInternalDep = "impossible-dep"
ppCheckExplanationId CIImpossibleInternalExe = "impossible-dep-exe"
ppCheckExplanationId CIMissingInternalExe = "no-internal-exe"
ppCheckExplanationId CINONELicense = "license-none"
ppCheckExplanationId CINoLicense = "no-license"
ppCheckExplanationId CIAllRightsReservedLicense = "all-rights-reserved"
ppCheckExplanationId CILicenseMessParse = "license-parse"
ppCheckExplanationId CIUnrecognisedLicense = "unknown-license"
ppCheckExplanationId CIUncommonBSD4 = "bsd4-license"
ppCheckExplanationId CIUnknownLicenseVersion = "unknown-license-version"
ppCheckExplanationId CINoLicenseFile = "no-license-file"
ppCheckExplanationId CIUnrecognisedSourceRepo = "unrecognised-repo-type"
ppCheckExplanationId CIMissingType = "repo-no-type"
ppCheckExplanationId CIMissingLocation = "repo-no-location"
ppCheckExplanationId CIMissingModule = "repo-no-module"
ppCheckExplanationId CIMissingTag = "repo-no-tag"
ppCheckExplanationId CISubdirRelPath = "repo-relative-dir"
ppCheckExplanationId CISubdirGoodRelPath = "repo-malformed-subdir"
ppCheckExplanationId CIOptFasm = "option-fasm"
ppCheckExplanationId CIOptHpc = "option-fhpc"
ppCheckExplanationId CIOptProf = "option-prof"
ppCheckExplanationId CIOptO = "option-o"
ppCheckExplanationId CIOptHide = "option-hide-package"
ppCheckExplanationId CIOptMake = "option-make"
ppCheckExplanationId CIOptONot = "option-optimize"
ppCheckExplanationId CIOptOOne = "option-o1"
ppCheckExplanationId CIOptOTwo = "option-o2"
ppCheckExplanationId CIOptSplitSections = "option-split-section"
ppCheckExplanationId CIOptSplitObjs = "option-split-objs"
ppCheckExplanationId CIOptWls = "option-optl-wl"
ppCheckExplanationId CIOptExts = "use-extension"
ppCheckExplanationId CIOptRts = "option-rtsopts"
ppCheckExplanationId CIOptWithRts = "option-with-rtsopts"
ppCheckExplanationId CICOptONumber = "option-opt-c"
ppCheckExplanationId CICOptCPP = "cpp-options"
ppCheckExplanationId CIOptAlternatives = "misplaced-c-opt"
ppCheckExplanationId CIRelativeOutside = "relative-path-outside"
ppCheckExplanationId CIAbsolutePath = "absolute-path"
ppCheckExplanationId CIBadRelativePath = "malformed-relative-path"
ppCheckExplanationId CIDistPoint = "unreliable-dist-path"
ppCheckExplanationId CIGlobSyntaxError = "glob-syntax-error"
ppCheckExplanationId CIRecursiveGlobInRoot = "recursive-glob"
ppCheckExplanationId CIInvalidOnWin = "invalid-path-win"
ppCheckExplanationId CIFilePathTooLong = "long-path"
ppCheckExplanationId CIFilePathNameTooLong = "long-name"
ppCheckExplanationId CIFilePathSplitTooLong = "name-not-portable"
ppCheckExplanationId CIFilePathEmpty = "empty-path"
ppCheckExplanationId CICVTestSuite = "test-cabal-ver"
ppCheckExplanationId CICVDefaultLanguage = "default-language"
ppCheckExplanationId CICVDefaultLanguageComponent = "no-default-language"
ppCheckExplanationId CICVDefaultLanguageComponentSoft = "add-language"
ppCheckExplanationId CICVExtraDocFiles = "extra-doc-files"
ppCheckExplanationId CICVMultiLib = "multilib"
ppCheckExplanationId CICVReexported = "reexported-modules"
ppCheckExplanationId CICVMixins = "mixins"
ppCheckExplanationId CICVExtraFrameworkDirs = "extra-framework-dirs"
ppCheckExplanationId CICVDefaultExtensions = "default-extensions"
ppCheckExplanationId CICVExtensionsDeprecated = "extensions-field"
ppCheckExplanationId CICVSources = "unsupported-sources"
ppCheckExplanationId CICVExtraDynamic = "extra-dynamic"
ppCheckExplanationId CICVVirtualModules = "virtual-modules"
ppCheckExplanationId CICVSourceRepository = "source-repository"
ppCheckExplanationId CICVExtensions = "incompatible-extension"
ppCheckExplanationId CICVCustomSetup = "no-setup-depends"
ppCheckExplanationId CICVExpliticDepsCustomSetup = "dependencies-setup"
ppCheckExplanationId CICVAutogenPaths = "no-autogen-paths"
ppCheckExplanationId CICVAutogenPackageInfo = "no-autogen-pinfo"
ppCheckExplanationId CICVAutogenPackageInfoGuard = "autogen-guard"
ppCheckExplanationId CIGlobNoMatch = "no-glob-match"
ppCheckExplanationId CIGlobExactMatch = "glob-no-extension"
ppCheckExplanationId CIGlobNoDir = "glob-missing-dir"
ppCheckExplanationId CIUnknownOS = "unknown-os"
ppCheckExplanationId CIUnknownArch = "unknown-arch"
ppCheckExplanationId CIUnknownCompiler = "unknown-compiler"
ppCheckExplanationId CIBaseNoUpperBounds = "missing-bounds-important"
ppCheckExplanationId CIMissingUpperBounds = "missing-upper-bounds"
ppCheckExplanationId CISuspiciousFlagName = "suspicious-flag"
ppCheckExplanationId CIDeclaredUsedFlags = "unused-flag"
ppCheckExplanationId CINonASCIICustomField = "non-ascii"
ppCheckExplanationId CIRebindableClashPaths = "rebindable-clash-paths"
ppCheckExplanationId CIRebindableClashPackageInfo = "rebindable-clash-info"
ppCheckExplanationId CIWErrorUnneeded = "werror"
ppCheckExplanationId CIJUnneeded = "unneeded-j"
ppCheckExplanationId CIFDeferTypeErrorsUnneeded = "fdefer-type-errors"
ppCheckExplanationId CIDynamicUnneeded = "debug-flag"
ppCheckExplanationId CIProfilingUnneeded = "fprof-flag"
ppCheckExplanationId CIUpperBoundSetup = "missing-bounds-setup"
ppCheckExplanationId CIDuplicateModule = "duplicate-modules"
ppCheckExplanationId CIPotentialDupModule = "maybe-duplicate-modules"
ppCheckExplanationId CIBOMStart = "bom"
ppCheckExplanationId CINotPackageName = "name-no-match"
ppCheckExplanationId CINoDesc = "no-cabal-file"
ppCheckExplanationId CIMultiDesc = "multiple-cabal-file"
ppCheckExplanationId CIUnknownFile = "unknown-file"
ppCheckExplanationId CIMissingSetupFile = "missing-setup"
ppCheckExplanationId CIMissingConfigureScript = "missing-conf-script"
ppCheckExplanationId CIUnknownDirectory = "unknown-directory"
ppCheckExplanationId CIMissingSourceControl = "no-repository"
ppCheckExplanationId CIMissingExpectedDocFiles = "no-docs"
ppCheckExplanationId CIWrongFieldForExpectedDocFiles = "doc-place"

-- String: the unrecognised 'CheckExplanationIDString' itself.
readExplanationID
  :: CheckExplanationIDString
  -> Either String CheckExplanationID
readExplanationID s = maybe (Left s) Right (lookup s idsDict)
  where
    idsDict :: [(CheckExplanationIDString, CheckExplanationID)]
    idsDict = map (\i -> (ppCheckExplanationId i, i)) [minBound .. maxBound]

-- | Which stanza does `CheckExplanation` refer to?
data CEType
  = CETLibrary LibraryName
  | CETForeignLibrary UnqualComponentName
  | CETExecutable UnqualComponentName
  | CETTest UnqualComponentName
  | CETBenchmark UnqualComponentName
  | CETSetup
  deriving (Eq, Ord, Show)

-- | Pretty printing `CEType`.
ppCET :: CEType -> String
ppCET cet = case cet of
  CETLibrary ln -> showLibraryName ln
  CETForeignLibrary n -> "foreign library" ++ qn n
  CETExecutable n -> "executable" ++ qn n
  CETTest n -> "test suite" ++ qn n
  CETBenchmark n -> "benchmark" ++ qn n
  CETSetup -> "custom-setup"
  where
    qn :: UnqualComponentName -> String
    qn wn = (" " ++) . quote . prettyShow $ wn

-- | Which language are we referring to in our warning message?
data WarnLang = LangC | LangCPlusPlus
  deriving (Eq, Ord, Show)

-- | Pretty printing `WarnLang`.
ppWarnLang :: WarnLang -> String
ppWarnLang LangC = "C"
ppWarnLang LangCPlusPlus = "C++"

-- | Pretty printing `CheckExplanation`.
ppExplanation :: CheckExplanation -> String
ppExplanation (ParseWarning fp pp) = showPWarning fp pp
ppExplanation NoNameField = "No 'name' field."
ppExplanation NoVersionField = "No 'version' field."
ppExplanation NoTarget =
  "No executables, libraries, tests, or benchmarks found. Nothing to do."
ppExplanation UnnamedInternal =
  "Found one or more unnamed internal libraries. Only the non-internal"
    ++ " library can have the same name as the package."
ppExplanation (DuplicateSections duplicateNames) =
  "Duplicate sections: "
    ++ commaSep (map unUnqualComponentName duplicateNames)
    ++ ". The name of every library, executable, test suite,"
    ++ " and benchmark section in the package must be unique."
ppExplanation (IllegalLibraryName pname) =
  "Illegal internal library name "
    ++ prettyShow pname
    ++ ". Internal libraries cannot have the same name as the package."
    ++ " Maybe you wanted a non-internal library?"
    ++ " If so, rewrite the section stanza"
    ++ " from 'library: '"
    ++ prettyShow pname
    ++ "' to 'library'."
ppExplanation (NoModulesExposed lName) =
  showLibraryName lName ++ " does not expose any modules"
ppExplanation SignaturesCabal2 =
  "To use the 'signatures' field the package needs to specify "
    ++ "at least 'cabal-version: 2.0'."
ppExplanation AutogenNotExposed =
  "An 'autogen-module' is neither on 'exposed-modules' nor 'other-modules'."
ppExplanation AutogenIncludesNotIncluded =
  "An include in 'autogen-includes' is neither in 'includes' nor "
    ++ "'install-includes'."
ppExplanation (NoMainIs eName) =
  "No 'main-is' field found for executable " ++ prettyShow eName
ppExplanation NoHsLhsMain =
  "The 'main-is' field must specify a '.hs' or '.lhs' file "
    ++ "(even if it is generated by a preprocessor), "
    ++ "or it may specify a C/C++/obj-C source file."
ppExplanation MainCCabal1_18 =
  "The package uses a C/C++/obj-C source file for the 'main-is' field. "
    ++ "To use this feature you need to specify 'cabal-version: 1.18' or"
    ++ " higher."
ppExplanation (AutogenNoOther ct) =
  "On "
    ++ ppCET ct
    ++ " an 'autogen-module'"
    ++ " is not on 'other-modules'"
ppExplanation AutogenIncludesNotIncludedExe =
  "An include in 'autogen-includes' is not in 'includes'."
ppExplanation (TestsuiteTypeNotKnown tt) =
  quote (prettyShow tt)
    ++ " is not a known type of test suite. "
    ++ "Either remove the 'type' field or use a known type. "
    ++ "The known test suite types are: "
    ++ commaSep (map prettyShow knownTestTypes)
ppExplanation (TestsuiteNotSupported tt) =
  quote (prettyShow tt)
    ++ " is not a supported test suite version. "
    ++ "Either remove the 'type' field or use a known type. "
    ++ "The known test suite types are: "
    ++ commaSep (map prettyShow knownTestTypes)
ppExplanation (BenchmarkTypeNotKnown tt) =
  quote (prettyShow tt)
    ++ " is not a known type of benchmark. "
    ++ "Either remove the 'type' field or use a known type. "
    ++ "The known benchmark types are: "
    ++ commaSep (map prettyShow knownBenchmarkTypes)
ppExplanation (BenchmarkNotSupported tt) =
  quote (prettyShow tt)
    ++ " is not a supported benchmark version. "
    ++ "Either remove the 'type' field or use a known type. "
    ++ "The known benchmark types are: "
    ++ commaSep (map prettyShow knownBenchmarkTypes)
ppExplanation NoHsLhsMainBench =
  "The 'main-is' field must specify a '.hs' or '.lhs' file "
    ++ "(even if it is generated by a preprocessor)."
ppExplanation (InvalidNameWin pkg) =
  "The package name '"
    ++ prettyShow pkg
    ++ "' is "
    ++ "invalid on Windows. Many tools need to convert package names to "
    ++ "file names, so using this name would cause problems."
ppExplanation ZPrefix =
  "Package names with the prefix 'z-' are reserved by Cabal and "
    ++ "cannot be used."
ppExplanation NoBuildType =
  "No 'build-type' specified. If you do not need a custom Setup.hs or "
    ++ "./configure script then use 'build-type: Simple'."
ppExplanation NoCustomSetup =
  "Ignoring the 'custom-setup' section because the 'build-type' is "
    ++ "not 'Custom'. Use 'build-type: Custom' if you need to use a "
    ++ "custom Setup.hs script."
ppExplanation (UnknownCompilers unknownCompilers) =
  "Unknown compiler "
    ++ commaSep (map quote unknownCompilers)
    ++ " in 'tested-with' field."
ppExplanation (UnknownLanguages unknownLanguages) =
  "Unknown languages: " ++ commaSep unknownLanguages
ppExplanation (UnknownExtensions unknownExtensions) =
  "Unknown extensions: " ++ commaSep unknownExtensions
ppExplanation (LanguagesAsExtension languagesUsedAsExtensions) =
  "Languages listed as extensions: "
    ++ commaSep languagesUsedAsExtensions
    ++ ". Languages must be specified in either the 'default-language' "
    ++ " or the 'other-languages' field."
ppExplanation (DeprecatedExtensions ourDeprecatedExtensions) =
  "Deprecated extensions: "
    ++ commaSep (map (quote . prettyShow . fst) ourDeprecatedExtensions)
    ++ ". "
    ++ unwords
      [ "Instead of '"
        ++ prettyShow ext
        ++ "' use '"
        ++ prettyShow replacement
        ++ "'."
      | (ext, Just replacement) <- ourDeprecatedExtensions
      ]
ppExplanation MissingFieldCategory = "No 'category' field."
ppExplanation MissingFieldMaintainer = "No 'maintainer' field."
ppExplanation MissingFieldSynopsis = "No 'synopsis' field."
ppExplanation MissingFieldDescription = "No 'description' field."
ppExplanation MissingFieldSynOrDesc = "No 'synopsis' or 'description' field."
ppExplanation SynopsisTooLong =
  "The 'synopsis' field is rather long (max 80 chars is recommended)."
ppExplanation ShortDesc =
  "The 'description' field should be longer than the 'synopsis' field. "
    ++ "It's useful to provide an informative 'description' to allow "
    ++ "Haskell programmers who have never heard about your package to "
    ++ "understand the purpose of your package. "
    ++ "The 'description' field content is typically shown by tooling "
    ++ "(e.g. 'cabal info', Haddock, Hackage) below the 'synopsis' which "
    ++ "serves as a headline. "
    ++ "Please refer to <https://cabal.readthedocs.io/en/stable/"
    ++ "cabal-package.html#package-properties> for more details."
ppExplanation (InvalidTestWith testedWithImpossibleRanges) =
  "Invalid 'tested-with' version range: "
    ++ commaSep (map prettyShow testedWithImpossibleRanges)
    ++ ". To indicate that you have tested a package with multiple "
    ++ "different versions of the same compiler use multiple entries, "
    ++ "for example 'tested-with: GHC==6.10.4, GHC==6.12.3' and not "
    ++ "'tested-with: GHC==6.10.4 && ==6.12.3'."
ppExplanation (ImpossibleInternalDep depInternalLibWithImpossibleVersion) =
  "The package has an impossible version range for a dependency on an "
    ++ "internal library: "
    ++ commaSep (map prettyShow depInternalLibWithImpossibleVersion)
    ++ ". This version range does not include the current package, and must "
    ++ "be removed as the current package's library will always be used."
ppExplanation (ImpossibleInternalExe depInternalExecWithImpossibleVersion) =
  "The package has an impossible version range for a dependency on an "
    ++ "internal executable: "
    ++ commaSep (map prettyShow depInternalExecWithImpossibleVersion)
    ++ ". This version range does not include the current package, and must "
    ++ "be removed as the current package's executable will always be used."
ppExplanation (MissingInternalExe depInternalExeWithImpossibleVersion) =
  "The package depends on a missing internal executable: "
    ++ commaSep (map prettyShow depInternalExeWithImpossibleVersion)
ppExplanation NONELicense = "The 'license' field is missing or is NONE."
ppExplanation NoLicense = "The 'license' field is missing."
ppExplanation AllRightsReservedLicense =
  "The 'license' is AllRightsReserved. Is that really what you want?"
ppExplanation (LicenseMessParse lic) =
  "Unfortunately the license "
    ++ quote (prettyShow lic)
    ++ " messes up the parser in earlier Cabal versions so you need to "
    ++ "specify 'cabal-version: >= 1.4'. Alternatively if you require "
    ++ "compatibility with earlier Cabal versions then use 'OtherLicense'."
ppExplanation (UnrecognisedLicense l) =
  quote ("license: " ++ l)
    ++ " is not a recognised license. The "
    ++ "known licenses are: "
    ++ commaSep (map prettyShow knownLicenses)
ppExplanation UncommonBSD4 =
  "Using 'license: BSD4' is almost always a misunderstanding. 'BSD4' "
    ++ "refers to the old 4-clause BSD license with the advertising "
    ++ "clause. 'BSD3' refers the new 3-clause BSD license."
ppExplanation (UnknownLicenseVersion lic known) =
  "'license: "
    ++ prettyShow lic
    ++ "' is not a known "
    ++ "version of that license. The known versions are "
    ++ commaSep (map prettyShow known)
    ++ ". If this is not a mistake and you think it should be a known "
    ++ "version then please file a ticket."
ppExplanation NoLicenseFile = "A 'license-file' is not specified."
ppExplanation (UnrecognisedSourceRepo kind) =
  quote kind
    ++ " is not a recognised kind of source-repository. "
    ++ "The repo kind is usually 'head' or 'this'"
ppExplanation MissingType =
  "The source-repository 'type' is a required field."
ppExplanation MissingLocation =
  "The source-repository 'location' is a required field."
ppExplanation MissingModule =
  "For a CVS source-repository, the 'module' is a required field."
ppExplanation MissingTag =
  "For the 'this' kind of source-repository, the 'tag' is a required "
    ++ "field. It should specify the tag corresponding to this version "
    ++ "or release of the package."
ppExplanation SubdirRelPath =
  "The 'subdir' field of a source-repository must be a relative path."
ppExplanation (SubdirGoodRelPath err) =
  "The 'subdir' field of a source-repository is not a good relative path: "
    ++ show err
ppExplanation (OptFasm fieldName) =
  "'"
    ++ fieldName
    ++ ": -fasm' is unnecessary and will not work on CPU "
    ++ "architectures other than x86, x86-64, ppc or sparc."
ppExplanation (OptHpc fieldName) =
  "'"
    ++ fieldName
    ++ ": -fhpc' is not necessary. Use the configure flag "
    ++ " --enable-coverage instead."
ppExplanation (OptProf fieldName) =
  "'"
    ++ fieldName
    ++ ": -prof' is not necessary and will lead to problems "
    ++ "when used on a library. Use the configure flag "
    ++ "--enable-library-profiling and/or --enable-profiling."
ppExplanation (OptO fieldName) =
  "'"
    ++ fieldName
    ++ ": -o' is not needed. "
    ++ "The output files are named automatically."
ppExplanation (OptHide fieldName) =
  "'"
    ++ fieldName
    ++ ": -hide-package' is never needed. "
    ++ "Cabal hides all packages."
ppExplanation (OptMake fieldName) =
  "'"
    ++ fieldName
    ++ ": --make' is never needed. Cabal uses this automatically."
ppExplanation (OptONot fieldName) =
  "'"
    ++ fieldName
    ++ ": -O0' is not needed. "
    ++ "Use the --disable-optimization configure flag."
ppExplanation (OptOOne fieldName) =
  "'"
    ++ fieldName
    ++ ": -O' is not needed. "
    ++ "Cabal automatically adds the '-O' flag. "
    ++ "Setting it yourself interferes with the --disable-optimization flag."
ppExplanation (OptOTwo fieldName) =
  "'"
    ++ fieldName
    ++ ": -O2' is rarely needed. "
    ++ "Check that it is giving a real benefit "
    ++ "and not just imposing longer compile times on your users."
ppExplanation (OptSplitSections fieldName) =
  "'"
    ++ fieldName
    ++ ": -split-sections' is not needed. "
    ++ "Use the --enable-split-sections configure flag."
ppExplanation (OptSplitObjs fieldName) =
  "'"
    ++ fieldName
    ++ ": -split-objs' is not needed. "
    ++ "Use the --enable-split-objs configure flag."
ppExplanation (OptWls fieldName) =
  "'"
    ++ fieldName
    ++ ": -optl-Wl,-s' is not needed and is not portable to"
    ++ " all operating systems. Cabal 1.4 and later automatically strip"
    ++ " executables. Cabal also has a flag --disable-executable-stripping"
    ++ " which is necessary when building packages for some Linux"
    ++ " distributions and using '-optl-Wl,-s' prevents that from working."
ppExplanation (OptExts fieldName) =
  "Instead of '"
    ++ fieldName
    ++ ": -fglasgow-exts' it is preferable to use "
    ++ "the 'extensions' field."
ppExplanation (OptRts fieldName) =
  "'"
    ++ fieldName
    ++ ": -rtsopts' has no effect for libraries. It should "
    ++ "only be used for executables."
ppExplanation (OptWithRts fieldName) =
  "'"
    ++ fieldName
    ++ ": -with-rtsopts' has no effect for libraries. It "
    ++ "should only be used for executables."
ppExplanation (COptONumber prefix label) =
  "'"
    ++ prefix
    ++ ": -O[n]' is generally not needed. When building with "
    ++ " optimisations Cabal automatically adds '-O2' for "
    ++ ppWarnLang label
    ++ " code. Setting it yourself interferes with the"
    ++ " --disable-optimization flag."
ppExplanation (COptCPP opt) =
  "'cpp-options: " ++ opt ++ "' is not a portable C-preprocessor flag."
ppExplanation (OptAlternatives badField goodField flags) =
  "Instead of "
    ++ quote (badField ++ ": " ++ unwords badFlags)
    ++ " use "
    ++ quote (goodField ++ ": " ++ unwords goodFlags)
  where
    (badFlags, goodFlags) = unzip flags
ppExplanation (RelativeOutside field path) =
  quote (field ++ ": " ++ path)
    ++ " is a relative path outside of the source tree. "
    ++ "This will not work when generating a tarball with 'sdist'."
ppExplanation (AbsolutePath field path) =
  quote (field ++ ": " ++ path)
    ++ " specifies an absolute path, but the "
    ++ quote field
    ++ " field must use relative paths."
ppExplanation (BadRelativePath field path err) =
  quote (field ++ ": " ++ path)
    ++ " is not a good relative path: "
    ++ show err
ppExplanation (DistPoint mfield path) =
  incipit
    ++ " points inside the 'dist' "
    ++ "directory. This is not reliable because the location of this "
    ++ "directory is configurable by the user (or package manager). In "
    ++ "addition, the layout of the 'dist' directory is subject to change "
    ++ "in future versions of Cabal."
  where
    -- mfiled Nothing -> the path is inside `ghc-options`
    incipit =
      maybe
        ("'ghc-options' path " ++ quote path)
        (\field -> quote (field ++ ": " ++ path))
        mfield
ppExplanation (GlobSyntaxError field expl) =
  "In the '" ++ field ++ "' field: " ++ expl
ppExplanation (RecursiveGlobInRoot field glob) =
  "In the '"
    ++ field
    ++ "': glob '"
    ++ glob
    ++ "' starts at project root directory, this might "
    ++ "include `.git/`, ``dist-newstyle/``, or other large directories!"
ppExplanation (InvalidOnWin paths) =
  "The "
    ++ quotes paths
    ++ " invalid on Windows, which "
    ++ "would cause portability problems for this package. Windows file "
    ++ "names cannot contain any of the characters \":*?<>|\", and there "
    ++ "are a few reserved names including \"aux\", \"nul\", \"con\", "
    ++ "\"prn\", \"com{1-9}\", \"lpt{1-9}\" and \"clock$\"."
  where
    quotes [failed] = "path " ++ quote failed ++ " is"
    quotes failed =
      "paths "
        ++ commaSep (map quote failed)
        ++ " are"
ppExplanation (FilePathTooLong path) =
  "The following file name is too long to store in a portable POSIX "
    ++ "format tar archive. The maximum length is 255 ASCII characters.\n"
    ++ "The file in question is:\n  "
    ++ path
ppExplanation (FilePathNameTooLong path) =
  "The following file name is too long to store in a portable POSIX "
    ++ "format tar archive. The maximum length for the name part (including "
    ++ "extension) is 100 ASCII characters. The maximum length for any "
    ++ "individual directory component is 155.\n"
    ++ "The file in question is:\n  "
    ++ path
ppExplanation (FilePathSplitTooLong path) =
  "The following file name is too long to store in a portable POSIX "
    ++ "format tar archive. While the total length is less than 255 ASCII "
    ++ "characters, there are unfortunately further restrictions. It has to "
    ++ "be possible to split the file path on a directory separator into "
    ++ "two parts such that the first part fits in 155 characters or less "
    ++ "and the second part fits in 100 characters or less. Basically you "
    ++ "have to make the file name or directory names shorter, or you could "
    ++ "split a long directory name into nested subdirectories with shorter "
    ++ "names.\nThe file in question is:\n  "
    ++ path
ppExplanation FilePathEmpty =
  "Encountered a file with an empty name, something is very wrong! "
    ++ "Files with an empty name cannot be stored in a tar archive or in "
    ++ "standard file systems."
ppExplanation CVTestSuite =
  "The 'test-suite' section is new in Cabal 1.10. "
    ++ "Unfortunately it messes up the parser in older Cabal versions "
    ++ "so you must specify at least 'cabal-version: >= 1.8', but note "
    ++ "that only Cabal 1.10 and later can actually run such test suites."
ppExplanation CVDefaultLanguage =
  "To use the 'default-language' field the package needs to specify "
    ++ "at least 'cabal-version: >= 1.10'."
ppExplanation CVDefaultLanguageComponent =
  "Packages using 'cabal-version: >= 1.10' and before 'cabal-version: 3.4' "
    ++ "must specify the 'default-language' field for each component (e.g. "
    ++ "Haskell98 or Haskell2010). If a component uses different languages "
    ++ "in different modules then list the other ones in the "
    ++ "'other-languages' field."
ppExplanation CVDefaultLanguageComponentSoft =
  "Without `default-language`, cabal will default to Haskell98, which is "
    ++ "probably not what you want. Please add `default-language` to all "
    ++ "targets."
ppExplanation CVExtraDocFiles =
  "To use the 'extra-doc-files' field the package needs to specify "
    ++ "'cabal-version: 1.18' or higher."
ppExplanation CVMultiLib =
  "To use multiple 'library' sections or a named library section "
    ++ "the package needs to specify at least 'cabal-version: 2.0'."
ppExplanation CVReexported =
  "To use the 'reexported-module' field the package needs to specify "
    ++ "'cabal-version: 1.22' or higher."
ppExplanation CVMixins =
  "To use the 'mixins' field the package needs to specify "
    ++ "at least 'cabal-version: 2.0'."
ppExplanation CVExtraFrameworkDirs =
  "To use the 'extra-framework-dirs' field the package needs to specify"
    ++ " 'cabal-version: 1.24' or higher."
ppExplanation CVDefaultExtensions =
  "To use the 'default-extensions' field the package needs to specify "
    ++ "at least 'cabal-version: >= 1.10'."
ppExplanation CVExtensionsDeprecated =
  "For packages using 'cabal-version: >= 1.10' the 'extensions' "
    ++ "field is deprecated. The new 'default-extensions' field lists "
    ++ "extensions that are used in all modules in the component, while "
    ++ "the 'other-extensions' field lists extensions that are used in "
    ++ "some modules, e.g. via the {-# LANGUAGE #-} pragma."
ppExplanation CVSources =
  "The use of 'asm-sources', 'cmm-sources', 'extra-bundled-libraries' "
    ++ " and 'extra-library-flavours' requires the package "
    ++ " to specify at least 'cabal-version: 3.0'."
ppExplanation (CVExtraDynamic flavs) =
  "The use of 'extra-dynamic-library-flavours' requires the package "
    ++ " to specify at least 'cabal-version: 3.0'. The flavours are: "
    ++ commaSep (concat flavs)
ppExplanation CVVirtualModules =
  "The use of 'virtual-modules' requires the package "
    ++ " to specify at least 'cabal-version: 2.2'."
ppExplanation CVSourceRepository =
  "The 'source-repository' section is new in Cabal 1.6. "
    ++ "Unfortunately it messes up the parser in earlier Cabal versions "
    ++ "so you need to specify 'cabal-version: >= 1.6'."
ppExplanation (CVExtensions version extCab12) =
  "Unfortunately the language extensions "
    ++ commaSep (map (quote . prettyShow) extCab12)
    ++ " break the parser in earlier Cabal versions so you need to "
    ++ "specify 'cabal-version: >= "
    ++ showCabalSpecVersion version
    ++ "'. Alternatively if you require compatibility with earlier "
    ++ "Cabal versions then you may be able to use an equivalent "
    ++ "compiler-specific flag."
ppExplanation CVCustomSetup =
  "Packages using 'cabal-version: 1.24' or higher with 'build-type: Custom' "
    ++ "must use a 'custom-setup' section with a 'setup-depends' field "
    ++ "that specifies the dependencies of the Setup.hs script itself. "
    ++ "The 'setup-depends' field uses the same syntax as 'build-depends', "
    ++ "so a simple example would be 'setup-depends: base, Cabal'."
ppExplanation CVExpliticDepsCustomSetup =
  "From version 1.24 cabal supports specifying explicit dependencies "
    ++ "for Custom setup scripts. Consider using 'cabal-version: 1.24' or "
    ++ "higher and adding a 'custom-setup' section with a 'setup-depends' "
    ++ "field that specifies the dependencies of the Setup.hs script "
    ++ "itself. The 'setup-depends' field uses the same syntax as "
    ++ "'build-depends', so a simple example would be 'setup-depends: base, "
    ++ "Cabal'."
ppExplanation CVAutogenPaths =
  "Packages using 'cabal-version: 2.0' and the autogenerated "
    ++ "module Paths_* must include it also on the 'autogen-modules' field "
    ++ "besides 'exposed-modules' and 'other-modules'. This specifies that "
    ++ "the module does not come with the package and is generated on "
    ++ "setup. Modules built with a custom Setup.hs script also go here "
    ++ "to ensure that commands like sdist don't fail."
ppExplanation CVAutogenPackageInfo =
  "Packages using 'cabal-version: 2.0' and the autogenerated "
    ++ "module PackageInfo_* must include it in 'autogen-modules' as well as"
    ++ " 'exposed-modules' and 'other-modules'. This specifies that "
    ++ "the module does not come with the package and is generated on "
    ++ "setup. Modules built with a custom Setup.hs script also go here "
    ++ "to ensure that commands like sdist don't fail."
ppExplanation CVAutogenPackageInfoGuard =
  "To use the autogenerated module PackageInfo_* you need to specify "
    ++ "`cabal-version: 3.12` or higher."
ppExplanation (GlobNoMatch field glob) =
  "In '"
    ++ field
    ++ "': the pattern '"
    ++ glob
    ++ "' does not"
    ++ " match any files."
ppExplanation (GlobExactMatch field glob file) =
  "In '"
    ++ field
    ++ "': the pattern '"
    ++ glob
    ++ "' does not"
    ++ " match the file '"
    ++ file
    ++ "' because the extensions do not"
    ++ " exactly match (e.g., foo.en.html does not exactly match *.html)."
    ++ " To enable looser suffix-only matching, set 'cabal-version: 2.4' or"
    ++ " higher."
ppExplanation (GlobNoDir field glob dir) =
  "In '"
    ++ field
    ++ "': the pattern '"
    ++ glob
    ++ "' attempts to"
    ++ " match files in the directory '"
    ++ dir
    ++ "', but there is no"
    ++ " directory by that name."
ppExplanation (UnknownOS unknownOSs) =
  "Unknown operating system name " ++ commaSep (map quote unknownOSs)
ppExplanation (UnknownArch unknownArches) =
  "Unknown architecture name " ++ commaSep (map quote unknownArches)
ppExplanation (UnknownCompiler unknownImpls) =
  "Unknown compiler name " ++ commaSep (map quote unknownImpls)
ppExplanation BaseNoUpperBounds =
  "The dependency 'build-depends: base' does not specify an upper "
    ++ "bound on the version number. Each major release of the 'base' "
    ++ "package changes the API in various ways and most packages will "
    ++ "need some changes to compile with it. The recommended practice "
    ++ "is to specify an upper bound on the version of the 'base' "
    ++ "package. This ensures your package will continue to build when a "
    ++ "new major version of the 'base' package is released. If you are "
    ++ "not sure what upper bound to use then use the next  major "
    ++ "version. For example if you have tested your package with 'base' "
    ++ "version 4.5 and 4.6 then use 'build-depends: base >= 4.5 && < 4.7'."
ppExplanation (MissingUpperBounds ct names) =
  let separator = "\n  - "
   in "On "
        ++ ppCET ct
        ++ ", "
        ++ "these packages miss upper bounds:"
        ++ separator
        ++ List.intercalate separator names
        ++ "\n"
        ++ "Please add them. There is more information at https://pvp.haskell.org/"
ppExplanation (SuspiciousFlagName invalidFlagNames) =
  "Suspicious flag names: "
    ++ unwords invalidFlagNames
    ++ ". "
    ++ "To avoid ambiguity in command line interfaces, a flag shouldn't "
    ++ "start with a dash. Also for better compatibility, flag names "
    ++ "shouldn't contain non-ascii characters."
ppExplanation (DeclaredUsedFlags declared used) =
  "Declared and used flag sets differ: "
    ++ s declared
    ++ " /= "
    ++ s used
    ++ ". "
  where
    s :: Set.Set FlagName -> String
    s = commaSep . map unFlagName . Set.toList
ppExplanation (NonASCIICustomField nonAsciiXFields) =
  "Non ascii custom fields: "
    ++ unwords nonAsciiXFields
    ++ ". "
    ++ "For better compatibility, custom field names "
    ++ "shouldn't contain non-ascii characters."
ppExplanation RebindableClashPaths =
  "Packages using RebindableSyntax with OverloadedStrings or"
    ++ " OverloadedLists in default-extensions, in conjunction with the"
    ++ " autogenerated module Paths_*, are known to cause compile failures"
    ++ " with Cabal < 2.2. To use these default-extensions with a Paths_*"
    ++ " autogen module, specify at least 'cabal-version: 2.2'."
ppExplanation RebindableClashPackageInfo =
  "Packages using RebindableSyntax with OverloadedStrings or"
    ++ " OverloadedLists in default-extensions, in conjunction with the"
    ++ " autogenerated module PackageInfo_*, are known to cause compile failures"
    ++ " with Cabal < 2.2. To use these default-extensions with a PackageInfo_*"
    ++ " autogen module, specify at least 'cabal-version: 2.2'."
ppExplanation (WErrorUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -Werror' makes the package easy to "
      ++ "break with future GHC versions because new GHC versions often "
      ++ "add new warnings."
ppExplanation (JUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -j[N]' can make sense for a particular user's setup,"
      ++ " but it is not appropriate for a distributed package."
ppExplanation (FDeferTypeErrorsUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -fdefer-type-errors' is fine during development "
      ++ "but is not appropriate for a distributed package."
ppExplanation (DynamicUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -d*' debug flags are not appropriate "
      ++ "for a distributed package."
ppExplanation (ProfilingUnneeded fieldName) =
  addConditionalExp $
    "'"
      ++ fieldName
      ++ ": -fprof*' profiling flags are typically not "
      ++ "appropriate for a distributed library package. These flags are "
      ++ "useful to profile this package, but when profiling other packages "
      ++ "that use this one these flags clutter the profile output with "
      ++ "excessive detail. If you think other packages really want to see "
      ++ "cost centres from this package then use '-fprof-auto-exported' "
      ++ "which puts cost centres only on exported functions."
ppExplanation (UpperBoundSetup nm) =
  "The dependency 'setup-depends: '"
    ++ nm
    ++ "' does not specify an "
    ++ "upper bound on the version number. Each major release of the "
    ++ "'"
    ++ nm
    ++ "' package changes the API in various ways and most "
    ++ "packages will need some changes to compile with it. If you are "
    ++ "not sure what upper bound to use then use the next major "
    ++ "version."
ppExplanation (DuplicateModule s dupLibsLax) =
  "Duplicate modules in "
    ++ s
    ++ ": "
    ++ commaSep (map prettyShow dupLibsLax)
ppExplanation (PotentialDupModule s dupLibsStrict) =
  "Potential duplicate modules (subject to conditionals) in "
    ++ s
    ++ ": "
    ++ commaSep (map prettyShow dupLibsStrict)
ppExplanation (BOMStart pdfile) =
  pdfile
    ++ " starts with an Unicode byte order mark (BOM)."
    ++ " This may cause problems with older cabal versions."
ppExplanation (NotPackageName pdfile expectedCabalname) =
  "The filename "
    ++ quote pdfile
    ++ " does not match package name "
    ++ "(expected: "
    ++ quote expectedCabalname
    ++ ")"
ppExplanation NoDesc =
  "No cabal file found.\n"
    ++ "Please create a package description file <pkgname>.cabal"
ppExplanation (MultiDesc multiple) =
  "Multiple cabal files found while checking.\n"
    ++ "Please use only one of: "
    ++ commaSep multiple
ppExplanation (UnknownFile fieldname file) =
  "The '"
    ++ fieldname
    ++ "' field refers to the file "
    ++ quote (getSymbolicPath file)
    ++ " which does not exist."
ppExplanation MissingSetupFile =
  "The package is missing a Setup.hs or Setup.lhs script."
ppExplanation MissingConfigureScript =
  "The 'build-type' is 'Configure' but there is no 'configure' script. "
    ++ "You probably need to run 'autoreconf -i' to generate it."
ppExplanation (UnknownDirectory kind dir) =
  quote (kind ++ ": " ++ dir)
    ++ " specifies a directory which does not exist."
ppExplanation MissingSourceControl =
  "When distributing packages, it is encouraged to specify source "
    ++ "control information in the .cabal file using one or more "
    ++ "'source-repository' sections. See the Cabal user guide for "
    ++ "details."
ppExplanation (MissingExpectedDocFiles extraDocFileSupport paths) =
  "Please consider including the "
    ++ quotes paths
    ++ " in the '"
    ++ targetField
    ++ "' section of the .cabal file "
    ++ "if it contains useful information for users of the package."
  where
    quotes [p] = "file " ++ quote p
    quotes ps = "files " ++ commaSep (map quote ps)
    targetField =
      if extraDocFileSupport
        then "extra-doc-files"
        else "extra-source-files"
ppExplanation (WrongFieldForExpectedDocFiles extraDocFileSupport field paths) =
  "Please consider moving the "
    ++ quotes paths
    ++ " from the '"
    ++ field
    ++ "' section of the .cabal file "
    ++ "to the section '"
    ++ targetField
    ++ "'."
  where
    quotes [p] = "file " ++ quote p
    quotes ps = "files " ++ commaSep (map quote ps)
    targetField =
      if extraDocFileSupport
        then "extra-doc-files"
        else "extra-source-files"

-- * Formatting utilities

commaSep :: [String] -> String
commaSep = List.intercalate ", "

quote :: String -> String
quote s = "'" ++ s ++ "'"

addConditionalExp :: String -> String
addConditionalExp expl =
  expl
    ++ " Alternatively, if you want to use this, make it conditional based "
    ++ "on a Cabal configuration flag (with 'manual: True' and 'default: "
    ++ "False') and enable that flag during development."
