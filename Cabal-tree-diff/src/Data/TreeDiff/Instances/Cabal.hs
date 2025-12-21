{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.TreeDiff.Instances.Cabal
  ( UnmergedGPD(..)
  , MergedGPD(..)
  )
  where

import Data.TreeDiff
import qualified Data.TreeDiff.OMap as OMap

import Data.TreeDiff.Instances.CabalLanguage ()
import Data.TreeDiff.Instances.CabalSPDX ()
import Data.TreeDiff.Instances.CabalVersion ()

-------------------------------------------------------------------------------

import Distribution.Backpack                       (OpenModule, OpenUnitId)
import Distribution.CabalSpecVersion               (CabalSpecVersion)
import Distribution.Compiler                       (CompilerFlavor, CompilerId, PerCompilerFlavor)
import Distribution.InstalledPackageInfo           (AbiDependency, ExposedModule, InstalledPackageInfo)
import Distribution.ModuleName                     (ModuleName)
import Distribution.PackageDescription
import Distribution.Simple.Compiler                (DebugInfoLevel, OptimisationLevel, ProfDetailLevel)
import Distribution.Simple.InstallDirs
import Distribution.Simple.InstallDirs.Internal
import Distribution.Simple.Setup                   (HaddockTarget, TestShowDetails)
import Distribution.System
import Distribution.Types.AbiHash                  (AbiHash)
import Distribution.Types.ComponentId              (ComponentId)
import Distribution.Types.DumpBuildInfo            (DumpBuildInfo)
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.UnitId                   (DefUnitId, UnitId)
import Distribution.Utils.NubList                  (NubList)
import Distribution.Utils.Path                     (SymbolicPathX)
import Distribution.Utils.ShortText                (ShortText, fromShortText)
import Distribution.Verbosity
import Distribution.Verbosity.Internal

import qualified Distribution.Compat.NonEmptySet as NES

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance (Eq a, Show a) => ToExpr (Condition a) where toExpr = defaultExprViaShow
instance (Show a, ToExpr b, ToExpr c, Show b, Show c, Eq a, Eq c, Eq b) => ToExpr (CondTree a b c)
instance (Show a, ToExpr b, ToExpr c, Show b, Show c, Eq a, Eq c, Eq b) => ToExpr (CondBranch a b c)
instance (ToExpr a) => ToExpr (NubList a)
instance ToExpr a => ToExpr (NES.NonEmptySet a) where
    toExpr xs = App "NonEmptySet.fromNonEmpty" [toExpr $ NES.toNonEmpty xs]

instance ToExpr a => ToExpr (PerCompilerFlavor a)

instance ToExpr Dependency where
    toExpr d@(Dependency pn vr cs)
        | cs == mainLibSet = App "Dependency" [toExpr pn, toExpr vr, App "mainLibSet" []]
        | otherwise        = genericToExpr d

instance ToExpr (SymbolicPathX allowAbs from to)

instance ToExpr a => ToExpr (InstallDirs a)

instance ToExpr a => ToExpr (WithImports a)

newtype MergedGPD = MergedGPD { getMergedGPD :: GenericPackageDescription }
newtype UnmergedGPD = UnmergedGPD { getUnmergedGPD :: GenericPackageDescription }

-- Note: The pattern matching is to ensure this doesn't go unnoticed when new fields are added.
instance ToExpr UnmergedGPD where
  toExpr
      ( getUnmergedGPD -> GenericPackageDescription'
        { packageDescriptionInternal
        , gpdScannedVersionInternal
        , genPackageFlagsInternal
        , gpdCommonStanzas
        , condLibraryUnmerged
        , condSubLibrariesUnmerged
        , condForeignLibsUnmerged
        , condExecutablesUnmerged
        , condTestSuitesUnmerged
        , condBenchmarksUnmerged
        }
      ) = Rec
            "GenericPackageDescription (internal, unmerged)"
            ( OMap.fromList
                [ ("packageDescriptionInternal", toExpr packageDescriptionInternal)
                , ("gpdScannedVersionInternal", toExpr gpdScannedVersionInternal)
                , ("genPackageFlagsInternal", toExpr genPackageFlagsInternal)
                , ("gpdCommonStanzas", toExpr gpdCommonStanzas)
                , ("condLibraryUnmerged", toExpr condLibraryUnmerged)
                , ("condSubLibrariesUnmerged", toExpr condSubLibrariesUnmerged)
                , ("condForeignLibsUnmerged", toExpr condForeignLibsUnmerged)
                , ("condExecutablesUnmerged", toExpr condExecutablesUnmerged)
                , ("condTestSuitesUnmerged", toExpr condTestSuitesUnmerged)
                , ("condBenchmarksUnmerged", toExpr condBenchmarksUnmerged)
                ]
            )


instance ToExpr MergedGPD where
  toExpr
      ( getMergedGPD -> GenericPackageDescription
        { packageDescription
        , gpdScannedVersion
        , genPackageFlags
        , condLibrary
        , condSubLibraries
        , condForeignLibs
        , condExecutables
        , condTestSuites
        , condBenchmarks
        }
      ) = Rec
            "GenericPackageDescription (merged)"
            ( OMap.fromList
                [ ("packageDescription", toExpr packageDescription)
                , ("gpdScannedVersion", toExpr gpdScannedVersion)
                , ("genPackageFlags", toExpr genPackageFlags)
                , ("condLibrary", toExpr condLibrary)
                , ("condSubLibraries", toExpr condSubLibraries)
                , ("condForeignLibs", toExpr condForeignLibs)
                , ("condExecutables", toExpr condExecutables)
                , ("condTestSuites", toExpr condTestSuites)
                , ("condBenchmarks", toExpr condBenchmarks)
                ]
            )

instance ToExpr AbiDependency
instance ToExpr AbiHash
instance ToExpr Arch
instance ToExpr Benchmark
instance ToExpr BenchmarkStanza
instance ToExpr BenchmarkInterface
instance ToExpr BenchmarkType
instance ToExpr BuildInfo
instance ToExpr BuildType
instance ToExpr CabalSpecVersion
instance ToExpr CompilerFlavor
instance ToExpr CompilerId
instance ToExpr ComponentId
instance ToExpr DebugInfoLevel
instance ToExpr DefUnitId
instance ToExpr DumpBuildInfo
instance ToExpr ExeDependency
instance ToExpr Executable
instance ToExpr ExecutableScope
instance ToExpr ExposedModule
instance ToExpr FlagAssignment
instance ToExpr FlagName
instance ToExpr ForeignLib
instance ToExpr ForeignLibOption
instance ToExpr ForeignLibType
instance ToExpr HaddockTarget
instance ToExpr IncludeRenaming
instance ToExpr InstalledPackageInfo
instance ToExpr KnownRepoType
instance ToExpr LegacyExeDependency
instance ToExpr LibVersionInfo
instance ToExpr Library
instance ToExpr LibraryName
instance ToExpr LibraryVisibility
instance ToExpr Mixin
instance ToExpr ModuleName
instance ToExpr ModuleReexport
instance ToExpr ModuleRenaming
instance ToExpr OS
instance ToExpr OpenModule
instance ToExpr OpenUnitId
instance ToExpr OptimisationLevel
instance ToExpr PackageDescription
instance ToExpr PackageFlag
instance ToExpr PackageIdentifier
instance ToExpr PackageName
instance ToExpr PackageVersionConstraint
instance ToExpr PathComponent
instance ToExpr PathTemplate
instance ToExpr PathTemplateVariable
instance ToExpr PkgconfigDependency
instance ToExpr PkgconfigName
instance ToExpr PkgconfigVersion
instance ToExpr PkgconfigVersionRange
instance ToExpr ProfDetailLevel
instance ToExpr RepoKind
instance ToExpr RepoType
instance ToExpr SetupBuildInfo
instance ToExpr SourceRepo
instance ToExpr TestShowDetails
instance ToExpr TestSuite
instance ToExpr TestSuiteStanza
instance ToExpr TestSuiteInterface
instance ToExpr TestType
instance ToExpr UnitId
instance ToExpr UnqualComponentName
instance ToExpr Verbosity
instance ToExpr VerbosityFlag
instance ToExpr VerbosityLevel

instance ToExpr ShortText where toExpr = toExpr . fromShortText
