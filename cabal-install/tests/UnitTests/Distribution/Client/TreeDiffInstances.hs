{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.TreeDiffInstances () where

import Distribution.Compiler
import Distribution.Simple.Compiler                (DebugInfoLevel, OptimisationLevel, ProfDetailLevel)
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs
import Distribution.Simple.InstallDirs.Internal
import Distribution.Simple.Setup                   (HaddockTarget, TestShowDetails)
import Distribution.System
import Distribution.Types.Flag                     (FlagAssignment, FlagName)
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
import Distribution.Client.IndexUtils.ActiveRepos
import Distribution.Client.IndexUtils.IndexState
import Distribution.Client.IndexUtils.Timestamp
import Distribution.Client.InstallSymlink
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Targets
import Distribution.Client.Types
import Distribution.Client.Types.SourceRepo              (SourceRepositoryPackage)

import UnitTests.Distribution.Client.GenericInstances ()

import Data.TreeDiff.Class
import Network.URI

instance (ToExpr k, ToExpr v) => ToExpr (MapMappend k v)
instance (ToExpr k, ToExpr v) => ToExpr (MapLast k v)
instance (ToExpr a) => ToExpr (NubList a)
instance (ToExpr a) => ToExpr (Flag a)

instance ToExpr (f FilePath) => ToExpr (SourceRepositoryPackage f)

instance ToExpr ActiveRepoEntry
instance ToExpr ActiveRepos
instance ToExpr AllowBootLibInstalls
instance ToExpr AllowNewer
instance ToExpr AllowOlder
instance ToExpr Arch
instance ToExpr BuildReport
instance ToExpr ClientInstallFlags
instance ToExpr CombineStrategy
instance ToExpr CompilerFlavor
instance ToExpr CompilerId
instance ToExpr ConstraintSource
instance ToExpr CountConflicts
instance ToExpr DebugInfoLevel
instance ToExpr FineGrainedConflicts
instance ToExpr FlagAssignment
instance ToExpr FlagName where toExpr = defaultExprViaShow
instance ToExpr HaddockTarget
instance ToExpr IndependentGoals
instance ToExpr InstallMethod
instance ToExpr InstallOutcome
instance ToExpr KnownRepoType
instance ToExpr LocalRepo
instance ToExpr MinimizeConflictSet
instance ToExpr OnlyConstrained
instance ToExpr OptimisationLevel
instance ToExpr OptionalStanza
instance ToExpr OS
instance ToExpr Outcome
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
instance ToExpr RelaxDeps
instance ToExpr RelaxDepScope
instance ToExpr RelaxDepSubject
instance ToExpr RelaxedDep
instance ToExpr RemoteRepo
instance ToExpr ReorderGoals
instance ToExpr RepoIndexState
instance ToExpr RepoKind
instance ToExpr RepoName
instance ToExpr ReportLevel
instance ToExpr RepoType
instance ToExpr ShortText
instance ToExpr SourceRepo
instance ToExpr StrongFlags
instance ToExpr TestShowDetails
instance ToExpr Timestamp
instance ToExpr TotalIndexState
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
