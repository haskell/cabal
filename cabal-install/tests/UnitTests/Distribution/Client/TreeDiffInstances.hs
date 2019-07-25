{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.TreeDiffInstances () where

import Distribution.Compiler
import Distribution.Simple.Compiler
  ( ProfDetailLevel, OptimisationLevel, DebugInfoLevel )
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.InstallDirs.Internal
import Distribution.Simple.Setup (HaddockTarget, TestShowDetails)
import Distribution.Types.GenericPackageDescription (FlagName, FlagAssignment)
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.SourceRepo
import Distribution.Types.Version
import Distribution.Types.VersionRange.Internal
import Distribution.Utils.NubList
import Distribution.Utils.ShortText
import Distribution.Verbosity
import Distribution.Verbosity.Internal

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.Settings

import Distribution.Client.BuildReports.Types
import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.Dependency.Types
import Distribution.Client.IndexUtils.Timestamp
import Distribution.Client.InstallSymlink
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Targets
import Distribution.Client.SourceRepo (SourceRepositoryPackage)
import Distribution.Client.Types

import UnitTests.Distribution.Client.GenericInstances ()

import Network.URI
import Data.TreeDiff.Class

instance (ToExpr k, ToExpr v) => ToExpr (MapMappend k v)
instance (ToExpr k, ToExpr v) => ToExpr (MapLast k v)
instance (ToExpr a) => ToExpr (NubList a)
instance (ToExpr a) => ToExpr (Flag a)

instance ToExpr AllowBootLibInstalls
instance ToExpr AllowNewer
instance ToExpr AllowOlder
instance ToExpr ClientInstallFlags
instance ToExpr CompilerFlavor
instance ToExpr ConstraintSource
instance ToExpr CountConflicts
instance ToExpr DebugInfoLevel
instance ToExpr FlagAssignment
instance ToExpr FlagName where toExpr = defaultExprViaShow
instance ToExpr HaddockTarget
instance ToExpr IndependentGoals
instance ToExpr IndexState
instance ToExpr InstallMethod
instance ToExpr MinimizeConflictSet
instance ToExpr OnlyConstrained
instance ToExpr OptimisationLevel
instance ToExpr OptionalStanza
instance ToExpr OverwritePolicy
instance ToExpr PackageConfig
instance ToExpr PackageIdentifier
instance ToExpr PackageName where toExpr = defaultExprViaShow
instance ToExpr PackageProperty
instance ToExpr PackageVersionConstraint
instance ToExpr PathComponent
instance ToExpr PathTemplate
instance ToExpr PathTemplateVariable
instance ToExpr PreSolver
instance ToExpr ProfDetailLevel
instance ToExpr ProjectConfig
instance ToExpr ProjectConfigBuildOnly
instance ToExpr ProjectConfigProvenance
instance ToExpr ProjectConfigShared
instance ToExpr RelaxDepMod
instance ToExpr RelaxDepScope
instance ToExpr RelaxDepSubject
instance ToExpr RelaxDeps
instance ToExpr RelaxedDep
instance ToExpr RemoteRepo
instance ToExpr ReorderGoals
instance ToExpr RepoKind
instance ToExpr RepoType
instance ToExpr ReportLevel
instance ToExpr ShortText
instance ToExpr SourceRepo
instance ToExpr (f FilePath) => ToExpr (SourceRepositoryPackage f)
instance ToExpr StrongFlags
instance ToExpr TestShowDetails
instance ToExpr Timestamp
instance ToExpr URI
instance ToExpr URIAuth
instance ToExpr UserConstraint
instance ToExpr UserConstraintScope
instance ToExpr UserQualifier
instance ToExpr Verbosity
instance ToExpr VerbosityFlag
instance ToExpr VerbosityLevel
instance ToExpr Version where toExpr = defaultExprViaShow
instance ToExpr VersionRange
instance ToExpr WriteGhcEnvironmentFilesPolicy
