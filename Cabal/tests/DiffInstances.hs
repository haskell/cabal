{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -freduction-depth=0 -fno-warn-orphans #-}
module DiffInstances where

import           Generics.SOP.TH
import           StructDiff

-------------------------------------------------------------------------------

import           Distribution.Compiler           (CompilerFlavor)
import           Distribution.License            (License)
import           Distribution.ModuleName         (ModuleName)
import           Distribution.Package
                 (Dependency, PackageIdentifier, PackageName)
import           Distribution.Types.ForeignLib
import           Distribution.Types.ForeignLibOption
import           Distribution.Types.ForeignLibType
import           Distribution.PackageDescription
                 (Benchmark, BenchmarkInterface, BenchmarkType, BuildInfo,
                 BuildType, CondTree, Condition, Executable, Flag, FlagName,
                 GenericPackageDescription, Library, ModuleReexport,
                 ModuleRenaming, PackageDescription, RepoKind, RepoType,
                 SetupBuildInfo, SourceRepo, TestSuite, TestSuiteInterface,
                 TestType)
import Distribution.Types.IncludeRenaming (IncludeRenaming)
import           Distribution.Version            (Version, VersionRange)
import           Language.Haskell.Extension
                 (Extension, KnownExtension, Language)

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

deriveGeneric ''Benchmark
deriveGeneric ''BenchmarkInterface
deriveGeneric ''BenchmarkType
deriveGeneric ''BuildInfo
deriveGeneric ''BuildType
deriveGeneric ''CompilerFlavor
deriveGeneric ''CondTree
deriveGeneric ''Dependency
deriveGeneric ''Executable
deriveGeneric ''Extension
deriveGeneric ''Flag
deriveGeneric ''ForeignLib
deriveGeneric ''ForeignLibOption
deriveGeneric ''ForeignLibType
deriveGeneric ''GenericPackageDescription
deriveGeneric ''KnownExtension
deriveGeneric ''Language
deriveGeneric ''Library
deriveGeneric ''License
deriveGeneric ''ModuleReexport
deriveGeneric ''ModuleRenaming
deriveGeneric ''PackageDescription
deriveGeneric ''PackageIdentifier
deriveGeneric ''PackageName
deriveGeneric ''RepoKind
deriveGeneric ''RepoType
deriveGeneric ''SetupBuildInfo
deriveGeneric ''SourceRepo
deriveGeneric ''TestSuite
deriveGeneric ''TestSuiteInterface
deriveGeneric ''TestType
deriveGeneric ''VersionRange
deriveGeneric ''IncludeRenaming

instance (Eq a, Show a) => Diff (Condition a) where diff = eqDiff
instance (Show a, Diff b, Diff c, Show b, Show c, Eq a, Eq c, Eq b) => Diff (CondTree a b c)

instance Diff Benchmark
instance Diff BenchmarkInterface
instance Diff BenchmarkType
instance Diff BuildInfo
instance Diff BuildType
instance Diff CompilerFlavor
instance Diff Dependency
instance Diff Executable
instance Diff Extension
instance Diff Flag
instance Diff FlagName where diff = eqDiff
instance Diff ForeignLib
instance Diff ForeignLibType
instance Diff ForeignLibOption
instance Diff GenericPackageDescription
instance Diff KnownExtension
instance Diff Language
instance Diff Library
instance Diff License
instance Diff ModuleName where diff = eqDiff
instance Diff ModuleReexport
instance Diff ModuleRenaming
instance Diff PackageDescription
instance Diff PackageIdentifier
instance Diff PackageName where diff = eqDiff
instance Diff RepoKind
instance Diff RepoType
instance Diff SetupBuildInfo
instance Diff SourceRepo
instance Diff TestSuite
instance Diff TestSuiteInterface
instance Diff TestType
instance Diff Version where diff = eqDiff
instance Diff VersionRange
instance Diff IncludeRenaming 
