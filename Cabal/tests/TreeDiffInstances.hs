{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-} 
#else
{-# OPTIONS_GHC -fcontext-stack=151 #-} 
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TreeDiffInstances where

import Data.TreeDiff

-------------------------------------------------------------------------------

import Distribution.Backpack                  (OpenModule, OpenUnitId)
import Distribution.Compiler                  (CompilerFlavor)
import Distribution.InstalledPackageInfo      (AbiDependency, ExposedModule, InstalledPackageInfo)
import Distribution.License                   (License)
import Distribution.ModuleName                (ModuleName)
import Distribution.Package                   (Dependency, PackageIdentifier, PackageName)
import Distribution.PackageDescription
import Distribution.Types.AbiHash             (AbiHash)
import Distribution.Types.ComponentId         (ComponentId)
import Distribution.Types.CondTree
import Distribution.Types.ExecutableScope
import Distribution.Types.ExeDependency
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.IncludeRenaming     (IncludeRenaming)
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Mixin
import Distribution.Types.PkgconfigDependency
import Distribution.Types.UnitId              (DefUnitId, UnitId)
import Distribution.Types.UnqualComponentName
import Distribution.Version                   (Version, VersionRange)
import Language.Haskell.Extension             (Extension, KnownExtension, Language)

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------


instance (Eq a, Show a) => ToExpr (Condition a) where toExpr = defaultExprViaShow
instance (Show a, ToExpr b, ToExpr c, Show b, Show c, Eq a, Eq c, Eq b) => ToExpr (CondTree a b c)
instance (Show a, ToExpr b, ToExpr c, Show b, Show c, Eq a, Eq c, Eq b) => ToExpr (CondBranch a b c)

instance ToExpr AbiDependency where toExpr = defaultExprViaShow
instance ToExpr AbiHash where toExpr = defaultExprViaShow
instance ToExpr Benchmark
instance ToExpr BenchmarkInterface
instance ToExpr BenchmarkType
instance ToExpr BuildInfo
instance ToExpr BuildType
instance ToExpr CompilerFlavor
instance ToExpr ComponentId where toExpr = defaultExprViaShow
instance ToExpr DefUnitId
instance ToExpr Dependency
instance ToExpr ExeDependency where toExpr = defaultExprViaShow
instance ToExpr Executable
instance ToExpr ExecutableScope where toExpr = defaultExprViaShow
instance ToExpr ExposedModule where toExpr = defaultExprViaShow
instance ToExpr Extension
instance ToExpr Flag
instance ToExpr FlagName where toExpr = defaultExprViaShow
instance ToExpr ForeignLib
instance ToExpr ForeignLibOption
instance ToExpr ForeignLibType
instance ToExpr GenericPackageDescription
instance ToExpr IncludeRenaming
instance ToExpr InstalledPackageInfo
instance ToExpr KnownExtension
instance ToExpr Language
instance ToExpr LegacyExeDependency where toExpr = defaultExprViaShow
instance ToExpr LibVersionInfo where toExpr = defaultExprViaShow
instance ToExpr Library
instance ToExpr License
instance ToExpr Mixin where toExpr = defaultExprViaShow
instance ToExpr ModuleName where toExpr = defaultExprViaShow
instance ToExpr ModuleReexport
instance ToExpr ModuleRenaming
instance ToExpr OpenModule
instance ToExpr OpenUnitId
instance ToExpr PackageDescription
instance ToExpr PackageIdentifier
instance ToExpr PackageName where toExpr = defaultExprViaShow
instance ToExpr PkgconfigDependency where toExpr = defaultExprViaShow
instance ToExpr RepoKind
instance ToExpr RepoType
instance ToExpr SetupBuildInfo
instance ToExpr SourceRepo
instance ToExpr TestSuite
instance ToExpr TestSuiteInterface
instance ToExpr TestType
instance ToExpr UnitId where toExpr = defaultExprViaShow
instance ToExpr UnqualComponentName where toExpr = defaultExprViaShow
instance ToExpr Version where toExpr = defaultExprViaShow
instance ToExpr VersionRange
