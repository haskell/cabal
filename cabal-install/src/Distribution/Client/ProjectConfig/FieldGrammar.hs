{-# LANGUAGE OverloadedStrings #-}

-- | 'ProjectConfig' Field descriptions
module Distribution.Client.ProjectConfig.FieldGrammar
  ( projectConfigFieldGrammar
  ) where

import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..), ProjectConfigBuildOnly (..))
import Distribution.Client.Utils.Parsec
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
import Distribution.Simple.Flag
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Verbosity

projectConfigFieldGrammar :: ParsecFieldGrammar' ProjectConfig
projectConfigFieldGrammar =
  ProjectConfig
    <$> monoidalFieldAla "packages" (alaList' FSep Token') L.projectPackages
    <*> monoidalFieldAla "optional-packages" (alaList' FSep Token') L.projectPackagesOptional
    <*> pure mempty -- source-repository-package stanza
    <*> monoidalFieldAla "extra-packages" formatPackagesNamedList L.projectPackagesNamed
    <*> blurFieldGrammar L.projectConfigBuildOnly projectConfigBuildOnlyFieldGrammar
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty

formatPackagesNamedList :: [PackageVersionConstraint] -> List CommaVCat (Identity PackageVersionConstraint) PackageVersionConstraint
formatPackagesNamedList = alaList CommaVCat

projectConfigBuildOnlyFieldGrammar :: ParsecFieldGrammar' ProjectConfigBuildOnly
projectConfigBuildOnlyFieldGrammar =
  ProjectConfigBuildOnly
    <$> optionalFieldDef "verbose" L.projectConfigVerbosity (pure normal)
    <*> pure mempty -- cli flag: projectConfigDryRun
    <*> pure mempty -- cli flag: projectConfigOnlyDeps
    <*> pure mempty -- cli flag: projectConfigOnlyDownload
    <*> monoidalFieldAla "build-summary" (alaNubList VCat) L.projectConfigSummaryFile
    <*> optionalFieldDef "build-log" L.projectConfigLogFile mempty
    <*> pure mempty -- cli flag: projectConfigBuildReports
    <*> optionalFieldDef "report-planning-failure" L.projectConfigReportPlanningFailure mempty
    <*> monoidalFieldAla "symlink-bindir" (alaFlag FilePathNT) L.projectConfigSymlinkBinDir
    <*> monoidalFieldAla "jobs" (alaFlag NumJobs) L.projectConfigNumJobs
    <*> optionalFieldDef "keep-going" L.projectConfigKeepGoing mempty
    <*> optionalFieldDef "offline" L.projectConfigOfflineMode mempty
    <*> optionalFieldDef "haddock-keep-temp-files" L.projectConfigKeepTempFiles mempty
    <*> monoidalFieldAla "http-transport" (alaFlag Token) L.projectConfigHttpTransport
    <*> optionalFieldDef "ignore-expiry" L.projectConfigIgnoreExpiry mempty
    <*> monoidalFieldAla "remote-repo-cache" (alaFlag FilePathNT) L.projectConfigCacheDir
    <*> monoidalFieldAla "logs-dir" (alaFlag FilePathNT) L.projectConfigLogsDir
    <*> pure mempty
