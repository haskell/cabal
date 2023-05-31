{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.TreeDiffInstances () where

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
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Targets
import Distribution.Client.Types
import Distribution.Client.Types.OverwritePolicy (OverwritePolicy)
import Distribution.Client.Types.SourceRepo (SourceRepositoryPackage)

import Distribution.Simple.Compiler (PackageDB)

import Data.TreeDiff.Class
import Data.TreeDiff.Instances.Cabal ()
import Network.URI

instance (ToExpr k, ToExpr v) => ToExpr (MapMappend k v)
instance (ToExpr k, ToExpr v) => ToExpr (MapLast k v)

instance ToExpr (f FilePath) => ToExpr (SourceRepositoryPackage f)

instance ToExpr ActiveRepoEntry
instance ToExpr ActiveRepos
instance ToExpr AllowBootLibInstalls
instance ToExpr AllowNewer
instance ToExpr AllowOlder
instance ToExpr BuildReport
instance ToExpr ClientInstallFlags
instance ToExpr CombineStrategy
instance ToExpr ConstraintSource
instance ToExpr CountConflicts
instance ToExpr FineGrainedConflicts
instance ToExpr IndependentGoals
instance ToExpr InstallMethod
instance ToExpr InstallOutcome
instance ToExpr LocalRepo
instance ToExpr MinimizeConflictSet
instance ToExpr OnlyConstrained
instance ToExpr OptionalStanza
instance ToExpr Outcome
instance ToExpr OverwritePolicy
instance ToExpr PackageConfig
instance ToExpr PackageDB
instance ToExpr PackageProperty
instance ToExpr PreferOldest
instance ToExpr PreSolver
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
instance ToExpr RepoName
instance ToExpr ReportLevel
instance ToExpr StrongFlags
instance ToExpr Timestamp
instance ToExpr TotalIndexState
instance ToExpr UserConstraint
instance ToExpr UserConstraintScope
instance ToExpr UserQualifier
instance ToExpr WriteGhcEnvironmentFilesPolicy

instance ToExpr URI
instance ToExpr URIAuth
