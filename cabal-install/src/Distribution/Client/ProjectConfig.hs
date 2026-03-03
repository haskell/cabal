{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Handling project configuration.
module Distribution.Client.ProjectConfig
  ( -- * Types for project config
    ProjectConfig (..)
  , ProjectConfigToParse (..)
  , ProjectConfigBuildOnly (..)
  , ProjectConfigShared (..)
  , ProjectConfigSkeleton
  , ProjectConfigProvenance (..)
  , PackageConfig (..)
  , MapLast (..)
  , MapMappend (..)

    -- * Project root
  , findProjectRoot
  , getProjectRootUsability
  , ProjectRoot (..)
  , BadProjectRoot (..)
  , ProjectRootUsability (..)

    -- * Project config files
  , readProjectConfig
  , readGlobalConfig
  , readProjectLocalExtraConfig
  , readProjectLocalFreezeConfig
  , reportParseResult
  , showProjectConfig
  , withGlobalConfig
  , withProjectOrGlobalConfig
  , writeProjectLocalExtraConfig
  , writeProjectLocalFreezeConfig
  , writeProjectConfigFile
  , commandLineFlagsToProjectConfig
  , onlyTopLevelProvenance
  , readSourcePackageCabalFile
  , readSourcePackageCabalFile'
  , CabalFileParseError (..)
  , readProjectFileSkeleton
  , ProjectFileParser (..)
  , readProjectFileSkeletonLegacy
  , readProjectFileSkeletonParsec
  , readProjectFileSkeletonFallback
  , readProjectFileSkeletonCompare

    -- * Packages within projects
  , ProjectPackageLocation (..)
  , BadPackageLocations (..)
  , BadPackageLocation (..)
  , BadPackageLocationMatch (..)
  , findProjectPackages
  , fetchAndReadSourcePackages

    -- * Resolving configuration
  , lookupLocalPackageConfig
  , projectConfigWithBuilderRepoContext
  , projectConfigWithSolverRepoContext
  , SolverSettings (..)
  , resolveSolverSettings
  , BuildTimeSettings (..)
  , resolveBuildTimeSettings
  , resolveNumJobsSetting

    -- * Checking configuration
  , checkBadPerPackageCompilerPaths
  , BadPerPackageCompilerPaths (..)

    -- * Globals
  , maxNumFetchJobs
  ) where

import Data.Bifunctor (second)
import Distribution.Client.Compat.Prelude hiding (empty)
import Distribution.Parsec.Source
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , debug
  , dieWithException
  , maybeExit
  , notice
  , noticeDoc
  , ordNub
  , rawSystemIOWithEnv
  , warn
  )
import Text.PrettyPrint (cat, colon, comma, empty, hsep, nest, quotes, render, text, vcat)
import Prelude ()

import Distribution.Client.Glob
  ( isTrivialRootedGlob
  )
import Distribution.Client.JobControl
import Distribution.Client.ProjectConfig.Legacy
import qualified Distribution.Client.ProjectConfig.Parsec as Parsec
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.RebuildMonad
import Distribution.Client.VCS
  ( SourceRepoProblem (..)
  , VCS (..)
  , configureVCS
  , knownVCSs
  , syncSourceRepos
  , validateSourceRepos
  )
import Distribution.Fields.ParseResult

import Distribution.Client.BuildReports.Types
  ( ReportLevel (..)
  )
import Distribution.Client.Config
  ( getConfigFilePath
  , loadConfig
  )
import Distribution.Client.DistDirLayout
  ( CabalDirLayout (..)
  , DistDirLayout (..)
  , ProjectRoot (..)
  , defaultProjectFile
  )
import Distribution.Client.Errors.Parser
import Distribution.Client.GlobalFlags
  ( RepoContext (..)
  , withRepoContext'
  )
import Distribution.Client.HashValue
import Distribution.Client.HttpUtils
  ( HttpTransport
  , configureTransport
  , downloadURI
  , transportCheckHttps
  )
import Distribution.Client.Types

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.SourcePackage

import Distribution.Client.Errors
import Distribution.Client.Setup
  ( defaultMaxBackjumps
  , defaultSolver
  )
import Distribution.Client.SrcDist
  ( packageDirToSdist
  )
import Distribution.Client.Targets
import Distribution.Client.Types.SourceRepo
  ( SourceRepoList
  , SourceRepositoryPackage (..)
  , srpFanOut
  )
import Distribution.Client.Utils
  ( determineNumJobs
  )
import qualified Distribution.Deprecated.ParseUtils as OldParser
  ( locatedErrorMsg
  , showPWarning
  )
import qualified Distribution.Deprecated.ProjectParseUtils as OldParser
  ( ProjectParseResult (..)
  )
import Distribution.Fields
import Distribution.Package
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription
  )
import Distribution.Simple.Compiler
  ( Compiler
  , compilerInfo
  )
import Distribution.Simple.InstallDirs
  ( PathTemplate
  , fromPathTemplate
  , initialPathTemplateEnv
  , substPathTemplate
  , toPathTemplate
  )
import Distribution.Simple.Program
  ( ConfiguredProgram (..)
  )
import Distribution.Simple.Setup
  ( Flag
  , flagToList
  , flagToMaybe
  , fromFlag
  , fromFlagOrDefault
  , toFlag
  , pattern Flag
  )
import Distribution.System
  ( Platform
  )
import Distribution.Types.GenericPackageDescription
  ( GenericPackageDescription
  )
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint (..)
  )
import Distribution.Types.SourceRepo
  ( RepoType (..)
  )
import Distribution.Utils.Generic
  ( toUTF8BS
  , toUTF8LBS
  )
import Distribution.Utils.NubList
  ( fromNubList
  )
import Distribution.Verbosity
  ( makeVerbose
  , modifyVerbosityFlags
  )
import Distribution.Version

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Distribution.Client.GZipUtils as GZipUtils
import qualified Distribution.Client.Tar as Tar

import Control.Exception (handle)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

import Network.URI
  ( URI (..)
  , URIAuth (..)
  , parseAbsoluteURI
  , uriToString
  )
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getCurrentDirectory
  , getDirectoryContents
  , getHomeDirectory
  , pathIsSymbolicLink
  )
import System.FilePath hiding (combine)
import System.IO
  ( IOMode (ReadMode)
  , withBinaryFile
  )

import Distribution.Deprecated.ProjectParseUtils (ProjectParseError (..), ProjectParseWarning)
import Distribution.Solver.Types.ProjectConfigPath

----------------------------------------
-- Resolving configuration to settings
--

-- | Look up a 'PackageConfig' field in the 'ProjectConfig' for a specific
-- 'PackageName'. This returns the configuration that applies to all local
-- packages plus any package-specific configuration for this package.
lookupLocalPackageConfig
  :: (Semigroup a, Monoid a)
  => (PackageConfig -> a)
  -> ProjectConfig
  -> PackageName
  -> a
lookupLocalPackageConfig
  field
  ProjectConfig
    { projectConfigLocalPackages
    , projectConfigSpecificPackage
    }
  pkgname =
    field projectConfigLocalPackages
      <> maybe
        mempty
        field
        (Map.lookup pkgname (getMapMappend projectConfigSpecificPackage))

-- | Use a 'RepoContext' based on the 'BuildTimeSettings'.
projectConfigWithBuilderRepoContext
  :: Verbosity
  -> BuildTimeSettings
  -> (RepoContext -> IO a)
  -> IO a
projectConfigWithBuilderRepoContext verbosity BuildTimeSettings{..} =
  withRepoContext'
    verbosity
    buildSettingRemoteRepos
    buildSettingLocalNoIndexRepos
    buildSettingCacheDir
    buildSettingHttpTransport
    (Just buildSettingIgnoreExpiry)
    buildSettingProgPathExtra

-- | Use a 'RepoContext', but only for the solver. The solver does not use the
-- full facilities of the 'RepoContext' so we can get away with making one
-- that doesn't have an http transport. And that avoids having to have access
-- to the 'BuildTimeSettings'
projectConfigWithSolverRepoContext
  :: Verbosity
  -> ProjectConfigShared
  -> ProjectConfigBuildOnly
  -> (RepoContext -> IO a)
  -> IO a
projectConfigWithSolverRepoContext
  verbosity
  ProjectConfigShared{..}
  ProjectConfigBuildOnly{..} =
    withRepoContext'
      verbosity
      (fromNubList projectConfigRemoteRepos)
      (fromNubList projectConfigLocalNoIndexRepos)
      ( fromFlagOrDefault
          ( error
              "projectConfigWithSolverRepoContext: projectConfigCacheDir"
          )
          projectConfigCacheDir
      )
      (flagToMaybe projectConfigHttpTransport)
      (flagToMaybe projectConfigIgnoreExpiry)
      (fromNubList projectConfigProgPathExtra)

-- | Resolve the project configuration, with all its optional fields, into
-- 'SolverSettings' with no optional fields (by applying defaults).
resolveSolverSettings :: ProjectConfig -> SolverSettings
resolveSolverSettings
  ProjectConfig
    { projectConfigShared
    , projectConfigLocalPackages
    , projectConfigSpecificPackage
    } =
    SolverSettings{..}
    where
      -- TODO: [required eventually] some of these settings need validation, e.g.
      -- the flag assignments need checking.
      cabalPkgname = mkPackageName "Cabal"

      profilingDynamicConstraint =
        ( UserConstraint
            (UserAnySetupQualifier cabalPkgname)
            (PackagePropertyVersion $ orLaterVersion (mkVersion [3, 13, 0]))
        , ConstraintSourceProfiledDynamic
        )

      profDynEnabledGlobally = fromFlagOrDefault False (packageConfigProfShared projectConfigLocalPackages)

      profDynEnabledAnyLocally =
        or
          [ fromFlagOrDefault False (packageConfigProfShared ppc)
          | (_, ppc) <- Map.toList (getMapMappend projectConfigSpecificPackage)
          ]

      -- Add a setup.Cabal >= 3.13 constraint if prof+dyn is enabled globally
      -- or any package in the project enables it.
      -- Ideally we'd apply this constraint only on the closure of packages requiring prof+dyn,
      -- but that would require another solver run for marginal advantages that
      -- will further shrink as 3.13 is adopted.
      solverCabalLibConstraints =
        [profilingDynamicConstraint | profDynEnabledGlobally || profDynEnabledAnyLocally]

      solverSettingRemoteRepos = fromNubList projectConfigRemoteRepos
      solverSettingLocalNoIndexRepos = fromNubList projectConfigLocalNoIndexRepos
      solverSettingConstraints = solverCabalLibConstraints ++ projectConfigConstraints
      solverSettingPreferences = projectConfigPreferences
      solverSettingFlagAssignment = packageConfigFlagAssignment projectConfigLocalPackages
      solverSettingFlagAssignments =
        fmap
          packageConfigFlagAssignment
          (getMapMappend projectConfigSpecificPackage)
      solverSettingCabalVersion = flagToMaybe projectConfigCabalVersion
      solverSettingSolver = fromFlag projectConfigSolver
      solverSettingAllowOlder = fromMaybe mempty projectConfigAllowOlder
      solverSettingAllowNewer = fromMaybe mempty projectConfigAllowNewer
      solverSettingMaxBackjumps = case fromFlag projectConfigMaxBackjumps of
        n
          | n < 0 -> Nothing
          | otherwise -> Just n
      solverSettingReorderGoals = fromFlag projectConfigReorderGoals
      solverSettingCountConflicts = fromFlag projectConfigCountConflicts
      solverSettingFineGrainedConflicts = fromFlag projectConfigFineGrainedConflicts
      solverSettingMinimizeConflictSet = fromFlag projectConfigMinimizeConflictSet
      solverSettingStrongFlags = fromFlag projectConfigStrongFlags
      solverSettingAllowBootLibInstalls = fromFlag projectConfigAllowBootLibInstalls
      solverSettingOnlyConstrained = fromFlag projectConfigOnlyConstrained
      solverSettingIndexState = flagToMaybe projectConfigIndexState
      solverSettingActiveRepos = flagToMaybe projectConfigActiveRepos
      solverSettingIndependentGoals = fromFlag projectConfigIndependentGoals
      solverSettingPreferOldest = fromFlag projectConfigPreferOldest
      -- solverSettingShadowPkgs        = fromFlag projectConfigShadowPkgs
      -- solverSettingReinstall         = fromFlag projectConfigReinstall
      -- solverSettingAvoidReinstalls   = fromFlag projectConfigAvoidReinstalls
      -- solverSettingOverrideReinstall = fromFlag projectConfigOverrideReinstall
      -- solverSettingUpgradeDeps       = fromFlag projectConfigUpgradeDeps

      ProjectConfigShared{..} = defaults <> projectConfigShared

      defaults =
        mempty
          { projectConfigSolver = Flag defaultSolver
          , projectConfigAllowOlder = Just (AllowOlder mempty)
          , projectConfigAllowNewer = Just (AllowNewer mempty)
          , projectConfigMaxBackjumps = Flag defaultMaxBackjumps
          , projectConfigReorderGoals = Flag (ReorderGoals False)
          , projectConfigCountConflicts = Flag (CountConflicts True)
          , projectConfigFineGrainedConflicts = Flag (FineGrainedConflicts True)
          , projectConfigMinimizeConflictSet = Flag (MinimizeConflictSet False)
          , projectConfigStrongFlags = Flag (StrongFlags False)
          , projectConfigAllowBootLibInstalls = Flag (AllowBootLibInstalls False)
          , projectConfigOnlyConstrained = Flag OnlyConstrainedNone
          , projectConfigIndependentGoals = Flag (IndependentGoals False)
          , projectConfigPreferOldest = Flag (PreferOldest False)
          -- projectConfigShadowPkgs        = Flag False,
          -- projectConfigReinstall         = Flag False,
          -- projectConfigAvoidReinstalls   = Flag False,
          -- projectConfigOverrideReinstall = Flag False,
          -- projectConfigUpgradeDeps       = Flag False
          }

-- | Resolve the project configuration, with all its optional fields, into
-- 'BuildTimeSettings' with no optional fields (by applying defaults).
resolveBuildTimeSettings
  :: Verbosity
  -> CabalDirLayout
  -> ProjectConfig
  -> BuildTimeSettings
resolveBuildTimeSettings
  verbosity
  CabalDirLayout
    { cabalLogsDirectory
    }
  ProjectConfig
    { projectConfigShared =
      ProjectConfigShared
        { projectConfigRemoteRepos
        , projectConfigLocalNoIndexRepos
        , projectConfigProgPathExtra
        }
    , projectConfigBuildOnly
    } =
    BuildTimeSettings{..}
    where
      buildSettingDryRun = fromFlag projectConfigDryRun
      buildSettingOnlyDeps = fromFlag projectConfigOnlyDeps
      buildSettingOnlyDownload = fromFlag projectConfigOnlyDownload
      buildSettingSummaryFile = fromNubList projectConfigSummaryFile
      -- buildSettingLogFile       -- defined below, more complicated
      -- buildSettingLogVerbosity  -- defined below, more complicated
      buildSettingBuildReports = fromFlag projectConfigBuildReports
      buildSettingSymlinkBinDir = flagToList projectConfigSymlinkBinDir
      buildSettingNumJobs = resolveNumJobsSetting projectConfigUseSemaphore projectConfigNumJobs
      buildSettingKeepGoing = fromFlag projectConfigKeepGoing
      buildSettingOfflineMode = fromFlag projectConfigOfflineMode
      buildSettingKeepTempFiles = fromFlag projectConfigKeepTempFiles
      buildSettingRemoteRepos = fromNubList projectConfigRemoteRepos
      buildSettingLocalNoIndexRepos = fromNubList projectConfigLocalNoIndexRepos
      buildSettingCacheDir = fromFlag projectConfigCacheDir
      buildSettingHttpTransport = flagToMaybe projectConfigHttpTransport
      buildSettingIgnoreExpiry = fromFlag projectConfigIgnoreExpiry
      buildSettingReportPlanningFailure =
        fromFlag projectConfigReportPlanningFailure
      buildSettingProgPathExtra = fromNubList projectConfigProgPathExtra
      buildSettingHaddockOpen = False

      ProjectConfigBuildOnly{..} =
        defaults
          <> projectConfigBuildOnly

      defaults =
        mempty
          { projectConfigDryRun = toFlag False
          , projectConfigOnlyDeps = toFlag False
          , projectConfigOnlyDownload = toFlag False
          , projectConfigBuildReports = toFlag NoReports
          , projectConfigReportPlanningFailure = toFlag False
          , projectConfigKeepGoing = toFlag False
          , projectConfigOfflineMode = toFlag False
          , projectConfigKeepTempFiles = toFlag False
          , projectConfigIgnoreExpiry = toFlag False
          }

      -- The logging logic: what log file to use and what verbosity.
      --
      -- If the user has specified --remote-build-reporting=detailed, use the
      -- default log file location. If the --build-log option is set, use the
      -- provided location. Otherwise don't use logging, unless building in
      -- parallel (in which case the default location is used).
      --
      buildSettingLogFile
        :: Maybe
            ( Compiler
              -> Platform
              -> PackageId
              -> UnitId
              -> FilePath
            )
      buildSettingLogFile
        | useDefaultTemplate = Just (substLogFileName defaultTemplate)
        | otherwise = fmap substLogFileName givenTemplate

      defaultTemplate =
        toPathTemplate $
          cabalLogsDirectory
            </> "$compiler"
            </> "$libname"
            <.> "log"
      givenTemplate = flagToMaybe projectConfigLogFile

      useDefaultTemplate
        | buildSettingBuildReports == DetailedReports = True
        | isJust givenTemplate = False
        | isParallelBuild buildSettingNumJobs = True
        | otherwise = False

      substLogFileName
        :: PathTemplate
        -> Compiler
        -> Platform
        -> PackageId
        -> UnitId
        -> FilePath
      substLogFileName template compiler platform pkgid uid =
        fromPathTemplate (substPathTemplate env template)
        where
          env =
            initialPathTemplateEnv
              pkgid
              uid
              (compilerInfo compiler)
              platform

      -- If the user has specified --remote-build-reporting=detailed or
      -- --build-log, use more verbose logging.
      --
      buildSettingLogVerbosity :: Verbosity
      buildSettingLogVerbosity
        | overrideVerbosity = modifyVerbosityFlags makeVerbose verbosity
        | otherwise = verbosity

      overrideVerbosity :: Bool
      overrideVerbosity
        | buildSettingBuildReports == DetailedReports = True
        | isJust givenTemplate = True
        | isParallelBuild buildSettingNumJobs = False
        | otherwise = False

-- | Determine the number of jobs (ParStrat) from the project config
resolveNumJobsSetting
  :: Flag Bool
  -- ^ Whether to use a semaphore (-jsem)
  -> Flag (Maybe Int)
  -- ^ The number of jobs to run concurrently
  -> ParStratX Int
resolveNumJobsSetting projectConfigUseSemaphore projectConfigNumJobs =
  if fromFlag projectConfigUseSemaphore
    then UseSem (determineNumJobs projectConfigNumJobs)
    else case (determineNumJobs projectConfigNumJobs) of
      1 -> Serial
      n -> NumJobs (Just n)

---------------------------------------------
-- Reading and writing project config files
--

-- | Get @ProjectRootUsability@ of a given file
getProjectRootUsability :: FilePath -> IO ProjectRootUsability
getProjectRootUsability filePath = do
  exists <- doesFileExist filePath
  if exists
    then return ProjectRootUsabilityPresentAndUsable
    else do
      let isUsableAction =
            handle @IOException
              -- NOTE: if any IOException is raised, we assume the file does not exist.
              -- That is what happen when we call @pathIsSymbolicLink@ on a @FilePath@ that does not exist.
              (const $ pure False)
              ((||) <$> pathIsSymbolicLink filePath <*> doesPathExist filePath)
      isUnusable <- isUsableAction
      if isUnusable
        then return ProjectRootUsabilityPresentAndUnusable
        else return ProjectRootUsabilityNotPresent

-- | Find the root of this project.
--
-- The project directory will be one of the following:
--   1. @mprojectDir@ when present
--   2. The first directory containing @mprojectFile@/@cabal.project@, starting from the current directory
--      and recursively checking parent directories
--   3. The current directory
findProjectRoot
  :: Verbosity
  -> Maybe FilePath
  -- ^ Explicit project directory
  -> Maybe FilePath
  -- ^ Explicit project file
  -> IO (Either BadProjectRoot ProjectRoot)
findProjectRoot verbosity mprojectDir mprojectFile = do
  case mprojectDir of
    Nothing
      | Just file <- mprojectFile
      , isAbsolute file -> do
          warn verbosity $
            "Specifying an absolute path to the project file is deprecated."
              <> " Use --project-dir to set the project's directory."

          getProjectRootUsability file >>= \case
            ProjectRootUsabilityPresentAndUsable ->
              uncurry projectRoot
                =<< first dropTrailingPathSeparator . splitFileName <$> canonicalizePath file
            ProjectRootUsabilityNotPresent ->
              left (BadProjectRootExplicitFileNotFound file)
            ProjectRootUsabilityPresentAndUnusable ->
              left (BadProjectRootFileBroken file)
      | otherwise -> probeProjectRoot mprojectFile
    Just dir ->
      doesDirectoryExist dir >>= \case
        False -> left (BadProjectRootDirNotFound dir)
        True -> do
          projectDir <- canonicalizePath dir

          case mprojectFile of
            Nothing -> pure $ Right (ProjectRootExplicit projectDir defaultProjectFile)
            Just projectFile
              | isAbsolute projectFile ->
                  getProjectRootUsability projectFile >>= \case
                    ProjectRootUsabilityNotPresent ->
                      left (BadProjectRootAbsoluteFileNotFound projectFile)
                    ProjectRootUsabilityPresentAndUsable ->
                      Right . ProjectRootExplicitAbsolute dir <$> canonicalizePath projectFile
                    ProjectRootUsabilityPresentAndUnusable ->
                      left (BadProjectRootFileBroken projectFile)
              | otherwise ->
                  getProjectRootUsability (projectDir </> projectFile) >>= \case
                    ProjectRootUsabilityNotPresent ->
                      left (BadProjectRootDirFileNotFound dir projectFile)
                    ProjectRootUsabilityPresentAndUsable ->
                      projectRoot projectDir projectFile
                    ProjectRootUsabilityPresentAndUnusable ->
                      left (BadProjectRootFileBroken projectFile)
  where
    left = pure . Left

    projectRoot projectDir projectFile =
      pure $ Right (ProjectRootExplicit projectDir projectFile)

probeProjectRoot :: Maybe FilePath -> IO (Either BadProjectRoot ProjectRoot)
probeProjectRoot mprojectFile = do
  startdir <- System.Directory.getCurrentDirectory
  homedir <- getHomeDirectory
  probe startdir homedir
  where
    projectFileName :: String
    projectFileName = fromMaybe defaultProjectFile mprojectFile

    -- Search upwards. If we get to the users home dir or the filesystem root,
    -- then use the current dir
    probe :: FilePath -> String -> IO (Either BadProjectRoot ProjectRoot)
    probe startdir homedir = go startdir
      where
        go :: FilePath -> IO (Either BadProjectRoot ProjectRoot)
        go dir | isDrive dir || dir == homedir =
          case mprojectFile of
            Nothing -> return (Right (ProjectRootImplicit startdir))
            Just file -> return (Left (BadProjectRootExplicitFileNotFound file))
        go dir = do
          getProjectRootUsability (dir </> projectFileName) >>= \case
            ProjectRootUsabilityNotPresent ->
              go (takeDirectory dir)
            ProjectRootUsabilityPresentAndUsable ->
              return (Right $ ProjectRootExplicit dir projectFileName)
            ProjectRootUsabilityPresentAndUnusable ->
              return (Left $ BadProjectRootFileBroken projectFileName)

-- | Errors returned by 'findProjectRoot'.
data BadProjectRoot
  = BadProjectRootExplicitFileNotFound FilePath
  | BadProjectRootDirNotFound FilePath
  | BadProjectRootAbsoluteFileNotFound FilePath
  | BadProjectRootDirFileNotFound FilePath FilePath
  | BadProjectRootFileBroken FilePath
  deriving (Show, Eq)

instance Exception BadProjectRoot where
  displayException = renderBadProjectRoot

renderBadProjectRoot :: BadProjectRoot -> String
renderBadProjectRoot = \case
  BadProjectRootExplicitFileNotFound projectFile ->
    "The given project file '" ++ projectFile ++ "' does not exist."
  BadProjectRootDirNotFound dir ->
    "The given project directory '" <> dir <> "' does not exist."
  BadProjectRootAbsoluteFileNotFound file ->
    "The given project file '" <> file <> "' does not exist."
  BadProjectRootDirFileNotFound dir file ->
    "The given project directory/file combination '" <> dir </> file <> "' does not exist."
  BadProjectRootFileBroken file ->
    "The given project file '" <> file <> "' is broken. Is it a broken symbolic link?"

-- | State of the project file, encodes if the file can be used
data ProjectRootUsability
  = -- | The file is present and can be used
    ProjectRootUsabilityPresentAndUsable
  | -- | The file is present but can't be used (e.g. broken symlink)
    ProjectRootUsabilityPresentAndUnusable
  | -- | The file is not present
    ProjectRootUsabilityNotPresent
  deriving (Eq, Show)

withGlobalConfig
  :: Verbosity
  -- ^ verbosity
  -> Flag FilePath
  -- ^ @--cabal-config@
  -> (ProjectConfig -> IO a)
  -- ^ with global
  -> IO a
withGlobalConfig verbosity gcf with = do
  globalConfig <- runRebuild "" $ readGlobalConfig verbosity gcf
  with globalConfig

withProjectOrGlobalConfig
  :: Flag Bool
  -- ^ whether to ignore local project (--ignore-project flag)
  -> IO a
  -- ^ continuation with project
  -> IO a
  -- ^ continuation without project
  -> IO a
withProjectOrGlobalConfig (Flag True) _with without = do
  without
withProjectOrGlobalConfig _ignorePrj with without =
  withProjectOrGlobalConfig' with without

withProjectOrGlobalConfig'
  :: IO a
  -- ^ continuation with project
  -> IO a
  -- ^ continuation without project
  -> IO a
withProjectOrGlobalConfig' with without = do
  catch with $
    \case
      (BadPackageLocations prov locs)
        | prov == Set.singleton Implicit
        , let
            isGlobErr (BadLocGlobEmptyMatch _) = True
            isGlobErr _ = False
        , any isGlobErr locs -> do
            without
      err -> throwIO err

-- | Read all the config relevant for a project. This includes the project
-- file if any, plus other global config.
readProjectConfig
  :: Verbosity
  -> ProjectFileParser
  -> HttpTransport
  -> Flag Bool
  -- ^ @--ignore-project@
  -> Flag FilePath
  -> DistDirLayout
  -> Rebuild ProjectConfigSkeleton
readProjectConfig verbosity parserOption _ (Flag True) configFileFlag _ = do
  global <- singletonProjectConfigSkeleton <$> readGlobalConfig verbosity configFileFlag
  return (global <> singletonProjectConfigSkeleton defaultImplicitProjectConfig)
readProjectConfig verbosity parserOption httpTransport _ configFileFlag distDirLayout = do
  global <- singletonProjectConfigSkeleton <$> readGlobalConfig verbosity configFileFlag
  local <- readProjectLocalConfigOrDefault verbosity parserOption httpTransport distDirLayout
  freeze <- readProjectLocalFreezeConfig verbosity parserOption httpTransport distDirLayout
  extra <- readProjectLocalExtraConfig verbosity parserOption httpTransport distDirLayout
  return (global <> local <> freeze <> extra)

-- | Reads an explicit @cabal.project@ file in the given project root dir,
-- or returns the default project config for an implicitly defined project.
readProjectLocalConfigOrDefault
  :: Verbosity
  -> ProjectFileParser
  -> HttpTransport
  -> DistDirLayout
  -> Rebuild ProjectConfigSkeleton
readProjectLocalConfigOrDefault verbosity parserOption httpTransport distDirLayout = do
  let projectFile = distProjectFile distDirLayout ""
  usesExplicitProjectRoot <- liftIO $ doesFileExist projectFile
  if usesExplicitProjectRoot
    then do
      readProjectFileSkeleton parserOption verbosity httpTransport distDirLayout "" "project file"
    else do
      monitorFiles [monitorNonExistentFile projectFile]
      return (singletonProjectConfigSkeleton defaultImplicitProjectConfig)

defaultImplicitProjectConfig :: ProjectConfig
defaultImplicitProjectConfig =
  mempty
    { -- We expect a package in the current directory.
      projectPackages = ["./*.cabal"]
    , projectConfigProvenance = Set.singleton Implicit
    }

-- | Reads a @cabal.project.local@ file in the given project root dir,
-- or returns empty. This file gets written by @cabal configure@, or in
-- principle can be edited manually or by other tools.
readProjectLocalExtraConfig
  :: Verbosity
  -> ProjectFileParser
  -> HttpTransport
  -> DistDirLayout
  -> Rebuild ProjectConfigSkeleton
readProjectLocalExtraConfig verbosity parserOption httpTransport distDirLayout =
  readProjectFileSkeleton
    parserOption
    verbosity
    httpTransport
    distDirLayout
    "local"
    "project local configuration file"

-- | Reads a @cabal.project.freeze@ file in the given project root dir,
-- or returns empty. This file gets written by @cabal freeze@, or in
-- principle can be edited manually or by other tools.
readProjectLocalFreezeConfig
  :: Verbosity
  -> ProjectFileParser
  -> HttpTransport
  -> DistDirLayout
  -> Rebuild ProjectConfigSkeleton
readProjectLocalFreezeConfig verbosity parserOption httpTransport distDirLayout =
  readProjectFileSkeleton
    parserOption
    verbosity
    httpTransport
    distDirLayout
    "freeze"
    "project freeze file"

-- | Reads a named extended (with imports and conditionals) config file in the given project root dir, or returns empty.
-- This function is generic and can be used with the legacy or parsec parser, or a combination of both.
readProjectFileSkeletonGen :: Verbosity -> HttpTransport -> DistDirLayout -> String -> String -> (FilePath -> IO ProjectConfigSkeleton) -> Rebuild ProjectConfigSkeleton
readProjectFileSkeletonGen
  verbosity
  httpTransport
  dir
  extensionName
  extensionDescription
  parseConfig =
    do
      exists <- liftIO $ doesFileExist extensionFile
      if exists
        then do
          monitorFiles [monitorFileHashed extensionFile]
          pcs <- liftIO $ parseConfig extensionFile
          monitorFiles $ map monitorFileHashed (projectConfigPathRoot <$> projectSkeletonImports pcs)
          return pcs
        else do
          monitorFiles [monitorNonExistentFile extensionFile]
          return mempty
    where
      extensionFile = (distProjectFile dir) extensionName

-- There are 3 different variants of the project parsing function.
-- 1. readProjectFileSkeletonLegacy: always uses the legacy parser
-- 2. readProjectFileSkeletonParsec: always uses the parsec parser
-- 3. readProjectFileSkeletonFallback: uses the parsec parser, but if that fails, it falls back to the legacy parser.
-- 4. readProjectFileSkeletonCompare: Run both parsers, and compare the results to check they are the same.
--
--
-- correspondingly there are two "pure" functions to attempt to parse a project
-- file using the "legacy" or "parsec" parser.
--
-- 1. parseProjectFileSkeletonLegacy: parses a project file using the legacy parser
-- 2. parseProjectFileSkeletonParsec: parses a project file using the parsec parser
--
-- Errors are handled in each case by
--
-- 1. reportParseResult: reports legacy parse errors to the user
-- 2. reportParseResultParsec: reports parsec parse errors to the user

readProjectFileSkeleton :: ProjectFileParser -> Verbosity -> HttpTransport -> DistDirLayout -> String -> String -> Rebuild ProjectConfigSkeleton
readProjectFileSkeleton option =
  case option of
    LegacyParser -> readProjectFileSkeletonLegacy
    ParsecParser -> readProjectFileSkeletonParsec
    FallbackParser -> readProjectFileSkeletonFallback
    CompareParser -> readProjectFileSkeletonCompare

-- | Read a project file using the legacy parser.
readProjectFileSkeletonLegacy :: Verbosity -> HttpTransport -> DistDirLayout -> String -> String -> Rebuild ProjectConfigSkeleton
readProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription = do
  readProjectFileSkeletonGen verbosity httpTransport distDirLayout extensionName extensionDescription $ \fp -> do
    debug verbosity $ "Reading project file using the legacy parser"
    parseProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription fp
      >>= liftIO . reportParseResult verbosity extensionDescription fp

-- | Read a project file using the parsec parser, but if that fails, it falls back to the legacy parser.
readProjectFileSkeletonFallback :: Verbosity -> HttpTransport -> DistDirLayout -> String -> String -> Rebuild ProjectConfigSkeleton
readProjectFileSkeletonFallback verbosity httpTransport distDirLayout extensionName extensionDescription = do
  readProjectFileSkeletonGen verbosity httpTransport distDirLayout extensionName extensionDescription $ \fp -> do
    debug verbosity $ "Reading project file using the fallback parser"
    (res, bs) <- parseProjectFileSkeletonParsec verbosity httpTransport distDirLayout extensionName extensionDescription fp
    let (_, pres) = runParseResult res
    case pres of
      -- 1. Successful parse with parsec parser, handle the result as normal.
      Right{} -> liftIO $ reportParseResultParsec verbosity fp bs res
      -- 2. The parse failed with the parsec parser, fallback to the legacy parser.
      Left{} -> do
        lres <- parseProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription fp
        case lres of
          -- 3a. The legacy parser worked, but the parsec parser failed!
          -- Report a warning to the user that this happened.
          OldParser.ProjectParseOk{} -> do
            warn verbosity $ "The new parsec parser failed, but the legacy parser worked. This is unexpected, please report this as a bug.\nThe legacy parser will be removed in the next major version."
            liftIO $ reportParseResult verbosity extensionDescription fp lres
          -- 3b. The legacy parser failed as well, report the original error.
          OldParser.ProjectParseFailed{} -> do
            liftIO $ reportParseResultParsec verbosity fp bs res

-- | Read a project file using the parsec parser.
readProjectFileSkeletonParsec :: Verbosity -> HttpTransport -> DistDirLayout -> String -> String -> Rebuild ProjectConfigSkeleton
readProjectFileSkeletonParsec verbosity httpTransport distDirLayout extensionName extensionDescription = do
  readProjectFileSkeletonGen verbosity httpTransport distDirLayout extensionName extensionDescription $ \fp -> do
    debug verbosity $ "Reading project file using the parsec parser"
    (res, bs) <- parseProjectFileSkeletonParsec verbosity httpTransport distDirLayout extensionName extensionDescription fp
    liftIO $ reportParseResultParsec verbosity fp bs res

readProjectFileSkeletonCompare :: Verbosity -> HttpTransport -> DistDirLayout -> String -> String -> Rebuild ProjectConfigSkeleton
readProjectFileSkeletonCompare verbosity httpTransport distDirLayout extensionName extensionDescription = do
  readProjectFileSkeletonGen verbosity httpTransport distDirLayout extensionName extensionDescription $ \fp -> do
    debug verbosity $ "Reading project file using the comparative parser"
    (pres, bs) <- parseProjectFileSkeletonParsec verbosity httpTransport distDirLayout extensionName extensionDescription fp
    lres <- parseProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription fp
    let (_, ppres) = runParseResult pres
    case (lres, ppres) of
      -- 1. Both succeed, compare the results
      (OldParser.ProjectParseOk lwarns lpcs, Right ppcs) -> do
        unless (lpcs == ppcs) (dieWithException verbosity $ LegacyAndParsecParseResultsDiffer fp (show lpcs) (show ppcs))
        liftIO $ reportParseResultParsec verbosity fp bs pres
      -- 2. The legacy parser failed, but the parsec parser succeeded.
      -- Report a warning to the user that this happened.
      (OldParser.ProjectParseFailed{}, Right{}) -> do
        warn verbosity $ "The legacy parser failed, but the new parsec parser worked. This is unexpected, please report this as a bug.\nThe legacy parser will be removed in the next major version."
        liftIO $ reportParseResult verbosity extensionDescription fp lres
      -- 3. The legacy parser succeeded, but the parsec parser failed.
      -- Report a warning to the user that this happened.
      (OldParser.ProjectParseOk{}, Left{}) -> do
        warn verbosity $ "The new parsec parser failed, but the legacy parser worked. This is unexpected, please report this as a bug.\nThe legacy parser will be removed in the next major version."
        liftIO $ reportParseResult verbosity extensionDescription fp lres
      (OldParser.ProjectParseFailed{}, Left{}) -> do
        -- 4. Both failed, report the original error. We don't check that the same errors are reported.
        liftIO $ reportParseResultParsec verbosity fp bs pres

reportParseResultParsec
  :: Verbosity
  -> FilePath
  -> BS.ByteString
  -> Parsec.ParseResult ProjectFileSource a
  -> IO a
reportParseResultParsec verbosity fpath contents pr = do
  let (warnings, result) = runParseResult pr
  case result of
    Right x -> do
      let sortKey p = (pwarningSource p, pwarningPosition (pwarning p))
          sortedWarnings = sortBy (comparing sortKey) warnings
      reportProjectParseWarnings verbosity fpath (map (showPWarningWithSource . fmap renderProjectFileSource) sortedWarnings)
      return x
    Left (_, errors) -> do
      dieWithException verbosity $ ProjectConfigParseFailure $ ProjectConfigParseError errors warnings

-- | Reads a named extended (with imports and conditionals) config file in the given project root dir, or returns empty.
parseProjectFileSkeletonLegacy :: Verbosity -> HttpTransport -> DistDirLayout -> String -> String -> FilePath -> IO (OldParser.ProjectParseResult ProjectConfigSkeleton)
parseProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription extensionFile =
  parseProject extensionFile (distDownloadSrcDirectory distDirLayout) httpTransport verbosity . ProjectConfigToParse
    =<< BS.readFile extensionFile

parseProjectFileSkeletonParsec :: Verbosity -> HttpTransport -> DistDirLayout -> String -> String -> FilePath -> IO (Parsec.ParseResult ProjectFileSource ProjectConfigSkeleton, BS.ByteString)
parseProjectFileSkeletonParsec verbosity httpTransport distDirLayout extensionName extensionDescription extensionFile = do
  bs <- BS.readFile extensionFile
  res <- Parsec.parseProject extensionFile (distDownloadSrcDirectory distDirLayout) httpTransport verbosity $ ProjectConfigToParse bs
  return (res, bs)

-- | Render the 'ProjectConfig' format.
--
-- For the moment this is implemented in terms of a pretty printer for the
-- legacy configuration types, plus a conversion.
showProjectConfig :: ProjectConfig -> String
showProjectConfig =
  showLegacyProjectConfig . convertToLegacyProjectConfig

-- | Write a @cabal.project.local@ file in the given project root dir.
writeProjectLocalExtraConfig :: DistDirLayout -> ProjectConfig -> IO ()
writeProjectLocalExtraConfig DistDirLayout{distProjectFile} =
  writeProjectConfigFile (distProjectFile "local")

-- | Write a @cabal.project.freeze@ file in the given project root dir.
writeProjectLocalFreezeConfig :: DistDirLayout -> ProjectConfig -> IO ()
writeProjectLocalFreezeConfig DistDirLayout{distProjectFile} =
  writeProjectConfigFile (distProjectFile "freeze")

-- | Write in the @cabal.project@ format to the given file.
writeProjectConfigFile :: FilePath -> ProjectConfig -> IO ()
writeProjectConfigFile file =
  writeFile file . showProjectConfig

-- | Read the user's cabal-install config file.
readGlobalConfig :: Verbosity -> Flag FilePath -> Rebuild ProjectConfig
readGlobalConfig verbosity configFileFlag = do
  config <- liftIO (loadConfig verbosity configFileFlag)
  configFile <- liftIO (getConfigFilePath verbosity configFileFlag)
  monitorFiles [monitorFileHashed configFile]
  return (convertLegacyGlobalConfig config)

reportProjectParseWarningsLegacy :: Verbosity -> FilePath -> [ProjectParseWarning] -> IO ()
reportProjectParseWarningsLegacy verbosity projectFile warnings =
  let msgs =
        [ OldParser.showPWarning pFilename w
        | (p, w) <- warnings
        , let pFilename = fst $ unconsProjectConfigPath p
        ]
   in reportProjectParseWarnings verbosity projectFile msgs

reportProjectParseWarnings :: Verbosity -> FilePath -> [String] -> IO ()
reportProjectParseWarnings verbosity projectFile msgs =
  unless (null msgs) $
    noticeDoc verbosity $
      vcat
        [ (text "Warnings found while parsing the project file" <> comma) <+> (text (takeFileName projectFile) <> colon)
        , cat [nest 1 $ text "-" <+> text m | m <- ordNub msgs]
        ]

reportParseResult :: Verbosity -> String -> FilePath -> OldParser.ProjectParseResult ProjectConfigSkeleton -> IO ProjectConfigSkeleton
reportParseResult verbosity _filetype projectFile (OldParser.ProjectParseOk warnings x) = do
  reportProjectParseWarningsLegacy verbosity projectFile warnings
  return x
reportParseResult verbosity filetype projectFile (OldParser.ProjectParseFailed (ProjectParseError snippet rootOrImportee err)) = do
  let (line, msg) = OldParser.locatedErrorMsg err
  let errLineNo = maybe "" (\n -> ':' : show n) line
  let (sourceFile, provenance) =
        maybe
          (projectFile, empty)
          ( \p ->
              ( currentProjectConfigPath p
              , if isTopLevelConfigPath p then empty else docProjectConfigPath p
              )
          )
          rootOrImportee
  let doc = case snippet of
        Nothing -> vcat (text <$> lines msg)
        Just s ->
          vcat
            [ provenance
            , text "Failed to parse" <+> quotes (text s) <+> (text "with error" <> colon)
            , nest 2 $ hsep $ text <$> lines msg
            ]
  dieWithException verbosity $ ReportParseResult filetype sourceFile errLineNo doc

---------------------------------------------
-- Finding packages in the project
--

-- | The location of a package as part of a project. Local file paths are
-- either absolute (if the user specified it as such) or they are relative
-- to the project root.
data ProjectPackageLocation
  = ProjectPackageLocalCabalFile FilePath
  | ProjectPackageLocalDirectory FilePath FilePath -- dir and .cabal file
  | ProjectPackageLocalTarball FilePath
  | ProjectPackageRemoteTarball URI
  | ProjectPackageRemoteRepo SourceRepoList
  | ProjectPackageNamed PackageVersionConstraint
  deriving (Show)

-- | Exception thrown by 'findProjectPackages'.
data BadPackageLocations
  = BadPackageLocations (Set ProjectConfigProvenance) [BadPackageLocation]
  deriving (Show)

instance Exception BadPackageLocations where
  displayException = renderBadPackageLocations

-- TODO: [nice to have] custom exception subclass for Doc rendering, colour etc

data BadPackageLocation
  = BadPackageLocationFile BadPackageLocationMatch
  | BadLocGlobEmptyMatch String
  | BadLocGlobBadMatches String [BadPackageLocationMatch]
  | BadLocUnexpectedUriScheme String
  | BadLocUnrecognisedUri String
  | BadLocUnrecognised String
  deriving (Show)

data BadPackageLocationMatch
  = BadLocUnexpectedFile String
  | BadLocNonexistantFile String
  | BadLocDirNoCabalFile String
  | BadLocDirManyCabalFiles String
  deriving (Show)

renderBadPackageLocations :: BadPackageLocations -> String
renderBadPackageLocations (BadPackageLocations provenance bpls)
  -- There is no provenance information,
  -- render standard bad package error information.
  | Set.null provenance = renderErrors renderBadPackageLocation
  -- The configuration is implicit, render bad package locations
  -- using possibly specialized error messages.
  | Set.singleton Implicit == provenance =
      renderErrors renderImplicitBadPackageLocation
  -- The configuration contains both implicit and explicit provenance.
  -- This should not occur, and a message is output to assist debugging.
  | Implicit `Set.member` provenance =
      "Warning: both implicit and explicit configuration is present."
        ++ renderExplicit
  -- The configuration was read from one or more explicit path(s),
  -- list the locations and render the bad package error information.
  -- The intent is to supersede this with the relevant location information
  -- per package error.
  | otherwise = renderExplicit
  where
    renderErrors f = unlines (map f bpls)

    renderExplicit =
      "When using configuration from:\n"
        ++ render (nest 2 . docProjectConfigFiles $ mapMaybe getExplicit (Set.toList provenance))
        ++ "\nThe following errors occurred:\n"
        ++ render (nest 2 $ vcat ((text "-" <+>) . text <$> map renderBadPackageLocation bpls))

    getExplicit (Explicit path) = Just path
    getExplicit Implicit = Nothing

-- TODO: [nice to have] keep track of the config file (and src loc) packages
-- were listed, to use in error messages

-- | Render bad package location error information for the implicit
-- @cabal.project@ configuration.
--
-- TODO: This is currently not fully realized, with only one of the implicit
-- cases handled. More cases should be added with informative help text
-- about the issues related specifically when having no project configuration
-- is present.
renderImplicitBadPackageLocation :: BadPackageLocation -> String
renderImplicitBadPackageLocation bpl = case bpl of
  BadLocGlobEmptyMatch pkglocstr ->
    "No cabal.project file or cabal file matching the default glob '"
      ++ pkglocstr
      ++ "' was found.\n"
      ++ "Please create a package description file <pkgname>.cabal "
      ++ "or a cabal.project file referencing the packages you "
      ++ "want to build."
  _ -> renderBadPackageLocation bpl

renderBadPackageLocation :: BadPackageLocation -> String
renderBadPackageLocation bpl = case bpl of
  BadPackageLocationFile badmatch ->
    renderBadPackageLocationMatch badmatch
  BadLocGlobEmptyMatch pkglocstr ->
    "The package location glob '"
      ++ pkglocstr
      ++ "' does not match any files or directories."
  BadLocGlobBadMatches pkglocstr failures ->
    "The package location glob '"
      ++ pkglocstr
      ++ "' does not match any "
      ++ "recognised forms of package. "
      ++ concatMap ((' ' :) . renderBadPackageLocationMatch) failures
  BadLocUnexpectedUriScheme pkglocstr ->
    "The package location URI '"
      ++ pkglocstr
      ++ "' does not use a "
      ++ "supported URI scheme. The supported URI schemes are http, https and "
      ++ "file."
  BadLocUnrecognisedUri pkglocstr ->
    "The package location URI '"
      ++ pkglocstr
      ++ "' does not appear to "
      ++ "be a valid absolute URI."
  BadLocUnrecognised pkglocstr ->
    "The package location syntax '" ++ pkglocstr ++ "' is not recognised."

renderBadPackageLocationMatch :: BadPackageLocationMatch -> String
renderBadPackageLocationMatch bplm = case bplm of
  BadLocUnexpectedFile pkglocstr ->
    "The package location '"
      ++ pkglocstr
      ++ "' is not recognised. The "
      ++ "supported file targets are .cabal files, .tar.gz tarballs or package "
      ++ "directories (i.e. directories containing a .cabal file)."
  BadLocNonexistantFile pkglocstr ->
    "The package location '" ++ pkglocstr ++ "' does not exist."
  BadLocDirNoCabalFile pkglocstr ->
    "The package directory '"
      ++ pkglocstr
      ++ "' does not contain any "
      ++ ".cabal file."
  BadLocDirManyCabalFiles pkglocstr ->
    "The package directory '"
      ++ pkglocstr
      ++ "' contains multiple "
      ++ ".cabal files (which is not currently supported)."

-- | Determines the location of all packages mentioned in the project configuration.
--
-- Throws 'BadPackageLocations'.
findProjectPackages
  :: DistDirLayout
  -> ProjectConfig
  -> Rebuild [ProjectPackageLocation]
findProjectPackages
  DistDirLayout{distProjectRootDirectory}
  ProjectConfig{..} = do
    requiredPkgs <- findPackageLocations True projectPackages
    optionalPkgs <- findPackageLocations False projectPackagesOptional
    let repoPkgs = map ProjectPackageRemoteRepo projectPackagesRepo
        namedPkgs = map ProjectPackageNamed projectPackagesNamed

    return (concat [requiredPkgs, optionalPkgs, repoPkgs, namedPkgs])
    where
      findPackageLocations :: Bool -> [String] -> Rebuild [ProjectPackageLocation]
      findPackageLocations required pkglocstr = do
        (problems, pkglocs) <-
          partitionEithers <$> traverse (findPackageLocation required) pkglocstr
        unless (null problems) $
          liftIO $
            throwIO $
              BadPackageLocations projectConfigProvenance problems
        return (concat pkglocs)

      findPackageLocation
        :: Bool
        -> String
        -> Rebuild (Either BadPackageLocation [ProjectPackageLocation])
      findPackageLocation _required@True pkglocstr =
        -- strategy: try first as a file:// or http(s):// URL.
        -- then as a file glob (usually encompassing single file)
        -- finally as a single file, for files that fail to parse as globs
        checkIsUriPackage pkglocstr
          `mplusMaybeT` checkIsFileGlobPackage pkglocstr
          `mplusMaybeT` checkIsSingleFilePackage pkglocstr
          >>= maybe (return (Left (BadLocUnrecognised pkglocstr))) return
      findPackageLocation _required@False pkglocstr = do
        -- just globs for optional case
        res <- checkIsFileGlobPackage pkglocstr
        case res of
          Nothing -> return (Left (BadLocUnrecognised pkglocstr))
          Just (Left _) -> return (Right []) -- it's optional
          Just (Right pkglocs) -> return (Right pkglocs)

      checkIsUriPackage
        , checkIsFileGlobPackage
        , checkIsSingleFilePackage
          :: String
          -> Rebuild (Maybe (Either BadPackageLocation [ProjectPackageLocation]))
      checkIsUriPackage pkglocstr =
        case parseAbsoluteURI pkglocstr of
          Just
            uri@URI
              { uriScheme = scheme
              , uriAuthority = Just URIAuth{uriRegName = host}
              , uriPath = path
              , uriQuery = query
              , uriFragment = frag
              }
              | recognisedScheme && not (null host) ->
                  return (Just (Right [ProjectPackageRemoteTarball uri]))
              | scheme == "file:" && null host && null query && null frag ->
                  checkIsSingleFilePackage path
              | not recognisedScheme && not (null host) ->
                  return (Just (Left (BadLocUnexpectedUriScheme pkglocstr)))
              | recognisedScheme && null host ->
                  return (Just (Left (BadLocUnrecognisedUri pkglocstr)))
              where
                recognisedScheme =
                  scheme == "http:"
                    || scheme == "https:"
                    || scheme == "file:"
          _ -> return Nothing

      checkIsFileGlobPackage pkglocstr =
        case simpleParsec pkglocstr of
          Nothing -> return Nothing
          Just glob -> liftM Just $ do
            matches <- matchFileGlob glob
            case matches of
              []
                | isJust (isTrivialRootedGlob glob) ->
                    return
                      ( Left
                          ( BadPackageLocationFile
                              (BadLocNonexistantFile pkglocstr)
                          )
                      )
              [] -> return (Left (BadLocGlobEmptyMatch pkglocstr))
              _ -> do
                (failures, pkglocs) <-
                  partitionEithers
                    <$> traverse checkFilePackageMatch matches
                return $! case (failures, pkglocs) of
                  ([failure], [])
                    | isJust (isTrivialRootedGlob glob) ->
                        Left (BadPackageLocationFile failure)
                  (_, []) -> Left (BadLocGlobBadMatches pkglocstr failures)
                  _ -> Right pkglocs

      checkIsSingleFilePackage pkglocstr = do
        let filename = distProjectRootDirectory </> pkglocstr
        isFile <- liftIO $ doesFileExist filename
        isDir <- liftIO $ doesDirectoryExist filename
        if isFile || isDir
          then
            checkFilePackageMatch pkglocstr
              >>= either
                (return . Just . Left . BadPackageLocationFile)
                (return . Just . Right . (\x -> [x]))
          else return Nothing

      checkFilePackageMatch
        :: String
        -> Rebuild
            ( Either
                BadPackageLocationMatch
                ProjectPackageLocation
            )
      checkFilePackageMatch pkglocstr = do
        -- The pkglocstr may be absolute or may be relative to the project root.
        -- Either way, </> does the right thing here. We return relative paths if
        -- they were relative in the first place.
        let abspath = distProjectRootDirectory </> pkglocstr
        isFile <- liftIO $ doesFileExist abspath
        isDir <- liftIO $ doesDirectoryExist abspath
        parentDirExists <- case takeDirectory abspath of
          [] -> return False
          dir -> liftIO $ doesDirectoryExist dir
        case () of
          _
            | isDir ->
                do
                  matches <- matchFileGlob (globStarDotCabal pkglocstr)
                  case matches of
                    [cabalFile] ->
                      return
                        ( Right
                            ( ProjectPackageLocalDirectory
                                pkglocstr
                                cabalFile
                            )
                        )
                    [] -> return (Left (BadLocDirNoCabalFile pkglocstr))
                    _ -> return (Left (BadLocDirManyCabalFiles pkglocstr))
            | extensionIsTarGz pkglocstr ->
                return (Right (ProjectPackageLocalTarball pkglocstr))
            | takeExtension pkglocstr == ".cabal" ->
                return (Right (ProjectPackageLocalCabalFile pkglocstr))
            | isFile ->
                return (Left (BadLocUnexpectedFile pkglocstr))
            | parentDirExists ->
                return (Left (BadLocNonexistantFile pkglocstr))
            | otherwise ->
                return (Left (BadLocUnexpectedFile pkglocstr))

      extensionIsTarGz f =
        takeExtension f == ".gz"
          && takeExtension (dropExtension f) == ".tar"

-- | A glob to find all the cabal files in a directory.
--
-- For a directory @some/dir/@, this is a glob of the form @some/dir/\*.cabal@.
-- The directory part can be either absolute or relative.
globStarDotCabal :: FilePath -> RootedGlob
globStarDotCabal dir =
  RootedGlob
    (if isAbsolute dir then FilePathRoot root else FilePathRelative)
    ( foldr
        (\d -> GlobDir [Literal d])
        (GlobFile [WildCard, Literal ".cabal"])
        dirComponents
    )
  where
    (root, dirComponents) = fmap splitDirectories (splitDrive dir)

-- TODO: [code cleanup] use sufficiently recent transformers package
mplusMaybeT :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
mplusMaybeT ma mb = do
  mx <- ma
  case mx of
    Nothing -> mb
    Just x -> return (Just x)

-------------------------------------------------
-- Fetching and reading packages in the project
--

-- | Read the @.cabal@ files for a set of packages. For remote tarballs and
-- VCS source repos this also fetches them if needed.
--
-- Note here is where we convert from project-root relative paths to absolute
-- paths.
fetchAndReadSourcePackages
  :: Verbosity
  -> DistDirLayout
  -> Maybe Compiler
  -> ProjectConfigShared
  -> ProjectConfigBuildOnly
  -> [ProjectPackageLocation]
  -> Rebuild [PackageSpecifier (SourcePackage UnresolvedPkgLoc)]
fetchAndReadSourcePackages
  verbosity
  distDirLayout
  compiler
  projectConfigShared
  projectConfigBuildOnly
  pkgLocations = do
    pkgsLocalDirectory <-
      sequenceA
        [ readSourcePackageLocalDirectory verbosity dir cabalFile
        | location <- pkgLocations
        , (dir, cabalFile) <- projectPackageLocal location
        ]

    pkgsLocalTarball <-
      sequenceA
        [ readSourcePackageLocalTarball verbosity path
        | ProjectPackageLocalTarball path <- pkgLocations
        ]

    pkgsRemoteTarball <- do
      getTransport <-
        delayInitSharedResource $
          configureTransport
            verbosity
            progPathExtra
            preferredHttpTransport
      sequenceA
        [ fetchAndReadSourcePackageRemoteTarball
          verbosity
          distDirLayout
          getTransport
          uri
        | ProjectPackageRemoteTarball uri <- pkgLocations
        ]

    pkgsRemoteRepo <-
      syncAndReadSourcePackagesRemoteRepos
        verbosity
        distDirLayout
        compiler
        projectConfigShared
        projectConfigBuildOnly
        (fromFlag (projectConfigOfflineMode projectConfigBuildOnly))
        [repo | ProjectPackageRemoteRepo repo <- pkgLocations]

    let pkgsNamed =
          [ NamedPackage pkgname [PackagePropertyVersion verrange]
          | ProjectPackageNamed (PackageVersionConstraint pkgname verrange) <- pkgLocations
          ]

    return $
      concat
        [ pkgsLocalDirectory
        , pkgsLocalTarball
        , pkgsRemoteTarball
        , pkgsRemoteRepo
        , pkgsNamed
        ]
    where
      projectPackageLocal (ProjectPackageLocalDirectory dir file) = [(dir, file)]
      projectPackageLocal (ProjectPackageLocalCabalFile file) = [(dir, file)]
        where
          dir = takeDirectory file
      projectPackageLocal _ = []

      progPathExtra = fromNubList (projectConfigProgPathExtra projectConfigShared)
      preferredHttpTransport =
        flagToMaybe (projectConfigHttpTransport projectConfigBuildOnly)

-- | A helper for 'fetchAndReadSourcePackages' to handle the case of
-- 'ProjectPackageLocalDirectory' and 'ProjectPackageLocalCabalFile'.
-- We simply read the @.cabal@ file.
readSourcePackageLocalDirectory
  :: Verbosity
  -> FilePath
  -- ^ The package directory
  -> FilePath
  -- ^ The package @.cabal@ file
  -> Rebuild (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
readSourcePackageLocalDirectory verbosity dir cabalFile = do
  monitorFiles [monitorFileHashed cabalFile]
  root <- askRoot
  let location = LocalUnpackedPackage (root </> dir)
  liftIO $
    fmap (mkSpecificSourcePackage location)
      . readSourcePackageCabalFile verbosity cabalFile
      =<< BS.readFile (root </> cabalFile)

-- | A helper for 'fetchAndReadSourcePackages' to handle the case of
-- 'ProjectPackageLocalTarball'. We scan through the @.tar.gz@ file to find
-- the @.cabal@ file and read that.
readSourcePackageLocalTarball
  :: Verbosity
  -> FilePath
  -> Rebuild (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
readSourcePackageLocalTarball verbosity tarballFile = do
  monitorFiles [monitorFile tarballFile]
  root <- askRoot
  let location = LocalTarballPackage (root </> tarballFile)
  liftIO $
    fmap (mkSpecificSourcePackage location)
      . uncurry (readSourcePackageCabalFile verbosity)
      =<< extractTarballPackageCabalFile (root </> tarballFile)

-- | A helper for 'fetchAndReadSourcePackages' to handle the case of
-- 'ProjectPackageRemoteTarball'. We download the tarball to the dist src dir
-- and after that handle it like the local tarball case.
fetchAndReadSourcePackageRemoteTarball
  :: Verbosity
  -> DistDirLayout
  -> Rebuild HttpTransport
  -> URI
  -> Rebuild (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
fetchAndReadSourcePackageRemoteTarball
  verbosity
  DistDirLayout
    { distDownloadSrcDirectory
    }
  getTransport
  tarballUri =
    -- The tarball download is expensive so we use another layer of file
    -- monitor to avoid it whenever possible.
    rerunIfChanged verbosity monitor tarballUri $ do
      -- Download
      transport <- getTransport
      liftIO $ do
        transportCheckHttps verbosity transport tarballUri
        notice verbosity ("Downloading " ++ show tarballUri)
        createDirectoryIfMissingVerbose
          verbosity
          True
          distDownloadSrcDirectory
        _ <- downloadURI transport verbosity tarballUri tarballFile
        return ()

      -- Read
      monitorFiles [monitorFile tarballFile]
      let location = RemoteTarballPackage tarballUri tarballFile
      liftIO $
        fmap (mkSpecificSourcePackage location)
          . uncurry (readSourcePackageCabalFile verbosity)
          =<< extractTarballPackageCabalFile tarballFile
    where
      tarballStem :: FilePath
      tarballStem =
        distDownloadSrcDirectory
          </> localFileNameForRemoteTarball tarballUri
      tarballFile :: FilePath
      tarballFile = tarballStem <.> "tar.gz"

      monitor :: FileMonitor URI (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
      monitor = newFileMonitor (tarballStem <.> "cache")

-- | A helper for 'fetchAndReadSourcePackages' to handle all the cases of
-- 'ProjectPackageRemoteRepo'.
syncAndReadSourcePackagesRemoteRepos
  :: Verbosity
  -> DistDirLayout
  -> Maybe Compiler
  -> ProjectConfigShared
  -> ProjectConfigBuildOnly
  -> Bool
  -> [SourceRepoList]
  -> Rebuild [PackageSpecifier (SourcePackage UnresolvedPkgLoc)]
syncAndReadSourcePackagesRemoteRepos
  verbosity
  DistDirLayout{distDownloadSrcDirectory}
  compiler
  ProjectConfigShared
    { projectConfigProgPathExtra
    }
  ProjectConfigBuildOnly
    { projectConfigUseSemaphore
    , projectConfigNumJobs
    }
  offlineMode
  repos = do
    repos' <-
      either reportSourceRepoProblems return $
        validateSourceRepos repos

    -- All 'SourceRepo's grouped by referring to the "same" remote repo
    -- instance. So same location but can differ in commit/tag/branch/subdir.
    let reposByLocation
          :: Map
              (RepoType, String)
              [(SourceRepoList, RepoType)]
        reposByLocation =
          Map.fromListWith
            (++)
            [ ((rtype, rloc), [(repo, vcsRepoType vcs)])
            | (repo, rloc, rtype, vcs) <- repos'
            ]

    let progPathExtra = fromNubList projectConfigProgPathExtra
    getConfiguredVCS <- delayInitSharedResources $ \repoType ->
      let vcs = Map.findWithDefault (error $ "Unknown VCS: " ++ prettyShow repoType) repoType knownVCSs
       in configureVCS verbosity progPathExtra vcs

    concat
      <$> rerunConcurrentlyIfChanged
        verbosity
        (newJobControlFromParStrat verbosity compiler parStrat (Just maxNumFetchJobs))
        [ ( monitor
          , repoGroup'
          , do
              vcs' <- getConfiguredVCS repoType
              syncRepoGroupAndReadSourcePackages vcs' pathStem repoGroup'
          )
        | repoGroup@((primaryRepo, repoType) : _) <- Map.elems reposByLocation
        , let repoGroup' = map fst repoGroup
              pathStem =
                distDownloadSrcDirectory
                  </> localFileNameForRemoteRepo primaryRepo
              monitor
                :: FileMonitor
                    [SourceRepoList]
                    [PackageSpecifier (SourcePackage UnresolvedPkgLoc)]
              monitor = newFileMonitor (pathStem <.> "cache")
        ]
    where
      parStrat = resolveNumJobsSetting projectConfigUseSemaphore projectConfigNumJobs
      syncRepoGroupAndReadSourcePackages
        :: VCS ConfiguredProgram
        -> FilePath
        -> [SourceRepoList]
        -> Rebuild [PackageSpecifier (SourcePackage UnresolvedPkgLoc)]
      syncRepoGroupAndReadSourcePackages vcs pathStem repoGroup = do
        liftIO $
          createDirectoryIfMissingVerbose
            verbosity
            False
            distDownloadSrcDirectory

        -- For syncing we don't care about different 'SourceRepo' values that
        -- are just different subdirs in the same repo.
        -- Do not sync source repositories when `--offline` flag applied.
        if not offlineMode
          then
            syncSourceRepos
              verbosity
              vcs
              [ (repo, repoPath)
              | (repo, _, repoPath) <- repoGroupWithPaths
              ]
          else do
            liftIO . warn verbosity $ "--offline was specified, skipping sync of repositories:"
            liftIO . for_ repoGroupWithPaths $ \(repo, _, _) -> warn verbosity $ srpLocation repo

        -- Run post-checkout-command if it is specified
        for_ repoGroupWithPaths $ \(repo, _, repoPath) ->
          for_ (nonEmpty (srpCommand repo)) $ \(cmd :| args) -> liftIO $ do
            maybeExit $ rawSystemIOWithEnv verbosity cmd args (Just repoPath) Nothing Nothing Nothing Nothing

        -- But for reading we go through each 'SourceRepo' including its subdir
        -- value and have to know which path each one ended up in.
        sequenceA
          [ readPackageFromSourceRepo repoWithSubdir repoPath
          | (_, reposWithSubdir, repoPath) <- repoGroupWithPaths
          , repoWithSubdir <- NE.toList reposWithSubdir
          ]
        where
          -- So to do both things above, we pair them up here.
          repoGroupWithPaths
            :: [(SourceRepositoryPackage Proxy, NonEmpty (SourceRepositoryPackage Maybe), FilePath)]
          repoGroupWithPaths =
            zipWith
              (\(x, y) z -> (x, y, z))
              ( mapGroup
                  [ (repo{srpSubdir = Proxy}, repo)
                  | repo <- foldMap (NE.toList . srpFanOut) repoGroup
                  ]
              )
              repoPaths

          mapGroup :: Ord k => [(k, v)] -> [(k, NonEmpty v)]
          mapGroup = Map.toList . Map.fromListWith (<>) . map (second pure)

          -- The repos in a group are given distinct names by simple enumeration
          -- foo, foo-2, foo-3 etc
          repoPaths :: [FilePath]
          repoPaths =
            pathStem
              : [pathStem ++ "-" ++ show (i :: Int) | i <- [2 ..]]

      readPackageFromSourceRepo
        :: SourceRepositoryPackage Maybe
        -> FilePath
        -> Rebuild (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
      readPackageFromSourceRepo repo repoPath = do
        let packageDir :: FilePath
            packageDir = maybe repoPath (repoPath </>) (srpSubdir repo)

        entries <- liftIO $ getDirectoryContents packageDir
        -- TODO: dcoutts 2018-06-23: wrap exceptions
        case filter (\e -> takeExtension e == ".cabal") entries of
          [] -> liftIO $ throwIO $ NoCabalFileFound packageDir
          (_ : _ : _) -> liftIO $ throwIO $ MultipleCabalFilesFound packageDir
          [cabalFileName] -> do
            let cabalFilePath = packageDir </> cabalFileName
            monitorFiles [monitorFileHashed cabalFilePath]
            gpd <- liftIO $ readSourcePackageCabalFile verbosity cabalFilePath =<< BS.readFile cabalFilePath

            -- write sdist tarball, to repoPath-pgkid
            tarball <- liftIO $ packageDirToSdist verbosity gpd packageDir
            let tarballPath = repoPath ++ "-" ++ prettyShow (packageId gpd) ++ ".tar.gz"
            liftIO $ LBS.writeFile tarballPath tarball

            let location = RemoteSourceRepoPackage repo tarballPath
            return $ mkSpecificSourcePackage location gpd

      reportSourceRepoProblems :: [(SourceRepoList, SourceRepoProblem)] -> Rebuild a
      reportSourceRepoProblems = liftIO . dieWithException verbosity . ReportSourceRepoProblems . renderSourceRepoProblems

      renderSourceRepoProblems :: [(SourceRepoList, SourceRepoProblem)] -> String
      renderSourceRepoProblems = unlines . map show -- "TODO: the repo problems"

-- | Utility used by all the helpers of 'fetchAndReadSourcePackages' to make an
-- appropriate @'PackageSpecifier' ('SourcePackage' (..))@ for a given package
-- from a given location.
mkSpecificSourcePackage
  :: PackageLocation FilePath
  -> GenericPackageDescription
  -> PackageSpecifier (SourcePackage UnresolvedPkgLoc)
mkSpecificSourcePackage location pkg =
  SpecificSourcePackage
    SourcePackage
      { srcpkgPackageId = packageId pkg
      , srcpkgDescription = pkg
      , srcpkgSource = fmap Just location
      , srcpkgDescrOverride = Nothing
      }

-- | Wrapper for the @.cabal@ file parser. It reports warnings on higher
-- verbosity levels and throws 'CabalFileParseError' on failure.
readSourcePackageCabalFile
  :: Verbosity
  -> FilePath
  -> BS.ByteString
  -> IO GenericPackageDescription
readSourcePackageCabalFile verbosity pkgfilename content =
  readSourcePackageCabalFile' (warn verbosity) pkgfilename content

-- | Like `readSourcePackageCabalFile`, but the `warn` function is an argument.
--
-- This is used when reading @.cabal@ files in indexes, where warnings should
-- generally be ignored.
readSourcePackageCabalFile'
  :: (String -> IO ())
  -> FilePath
  -> BS.ByteString
  -> IO GenericPackageDescription
readSourcePackageCabalFile' logWarnings pkgfilename content =
  case runParseResult (withSource (PCabalFile (pkgfilename, content)) $ parseGenericPackageDescription content) of
    (warnings, Right pkg) -> do
      unless (null warnings) $
        logWarnings (formatWarnings . map (fmap renderCabalFileSource) $ warnings)
      return pkg
    (warnings, Left (mspecVersion, errors)) ->
      throwIO $ CabalFileParseError pkgfilename content errors mspecVersion warnings
  where
    formatWarnings warnings =
      "The package description file "
        ++ pkgfilename
        ++ " has warnings: "
        ++ unlines (map showPWarningWithSource warnings)

-- | When looking for a package's @.cabal@ file we can find none, or several,
-- both of which are failures.
data CabalFileSearchFailure
  = NoCabalFileFound FilePath
  | MultipleCabalFilesFound FilePath
  deriving (Show)

instance Exception CabalFileSearchFailure

-- | Find the @.cabal@ file within a tarball file and return it by value.
--
-- Can fail with a 'Tar.FormatError' or 'CabalFileSearchFailure' exception.
extractTarballPackageCabalFile :: FilePath -> IO (FilePath, BS.ByteString)
extractTarballPackageCabalFile tarballFile =
  withBinaryFile tarballFile ReadMode $ \hnd -> do
    content <- LBS.hGetContents hnd
    case extractTarballPackageCabalFilePure tarballFile content of
      Left (Left e) -> throwIO e
      Left (Right e) -> throwIO e
      Right (fileName, fileContent) ->
        (,) fileName <$> evaluate (LBS.toStrict fileContent)

-- | Scan through a tar file stream and collect the @.cabal@ file, or fail.
extractTarballPackageCabalFilePure
  :: FilePath
  -> LBS.ByteString
  -> Either
      ( Either
          Tar.FormatError
          CabalFileSearchFailure
      )
      (FilePath, LBS.ByteString)
extractTarballPackageCabalFilePure tarballFile =
  check
    . accumEntryMap
    . Tar.filterEntries isCabalFile
    . Tar.read
    . GZipUtils.maybeDecompress
  where
    accumEntryMap =
      Tar.foldlEntries
        (\m e -> Map.insert (Tar.entryTarPath e) e m)
        Map.empty

    check (Left (e, _m)) = Left (Left e)
    check (Right m) = case Map.elems m of
      [] -> Left (Right $ NoCabalFileFound tarballFile)
      [file] -> case Tar.entryContent file of
        Tar.NormalFile content _ -> Right (Tar.entryPath file, content)
        _ -> Left (Right $ NoCabalFileFound tarballFile)
      _files -> Left (Right $ MultipleCabalFilesFound tarballFile)

    isCabalFile e = case splitPath (Tar.entryPath e) of
      [_dir, file] -> takeExtension file == ".cabal"
      [".", _dir, file] -> takeExtension file == ".cabal"
      _ -> False

-- | The name to use for a local file for a remote tarball 'SourceRepo'.
-- This is deterministic based on the remote tarball URI, and is intended
-- to produce non-clashing file names for different tarballs.
localFileNameForRemoteTarball :: URI -> FilePath
localFileNameForRemoteTarball uri =
  mangleName uri
    ++ "-"
    ++ showHashValue locationHash
  where
    mangleName =
      truncateString 10
        . dropExtension
        . dropExtension
        . takeFileName
        . dropTrailingPathSeparator
        . uriPath

    locationHash :: HashValue
    locationHash = hashValue (toUTF8LBS (uriToString id uri ""))

-- | The name to use for a local file or dir for a remote 'SourceRepo'.
-- This is deterministic based on the source repo identity details, and
-- intended to produce non-clashing file names for different repos.
localFileNameForRemoteRepo :: SourceRepoList -> FilePath
localFileNameForRemoteRepo SourceRepositoryPackage{srpType, srpLocation} =
  mangleName srpLocation ++ "-" ++ showHashValue locationHash
  where
    mangleName =
      truncateString 10
        . dropExtension
        . takeFileName
        . dropTrailingPathSeparator

    -- just the parts that make up the "identity" of the repo
    locationHash :: HashValue
    locationHash =
      hashValue $
        LBS.fromChunks [toUTF8BS srpLocation, toUTF8BS (show srpType)]

-- | Truncate a string, with a visual indication that it is truncated.
truncateString :: Int -> String -> String
truncateString n s
  | length s <= n = s
  | otherwise = take (n - 1) s ++ "_"

-- TODO: add something like this, here or in the project planning
-- Based on the package location, which packages will be built inplace in the
-- build tree vs placed in the store. This has various implications on what we
-- can do with the package, e.g. can we run tests, ghci etc.
--
-- packageIsLocalToProject :: ProjectPackageLocation -> Bool

---------------------------------------------
-- Checking configuration sanity
--

data BadPerPackageCompilerPaths
  = BadPerPackageCompilerPaths [(PackageName, String)]
  deriving (Show)

instance Exception BadPerPackageCompilerPaths where
  displayException = renderBadPerPackageCompilerPaths

-- TODO: [nice to have] custom exception subclass for Doc rendering, colour etc

renderBadPerPackageCompilerPaths :: BadPerPackageCompilerPaths -> String
renderBadPerPackageCompilerPaths
  (BadPerPackageCompilerPaths ((pkgname, progname) : _)) =
    "The path to the compiler program (or programs used by the compiler) "
      ++ "cannot be specified on a per-package basis in the cabal.project file "
      ++ "(i.e. setting the '"
      ++ progname
      ++ "-location' for package '"
      ++ prettyShow pkgname
      ++ "'). All packages have to use the same compiler, so "
      ++ "specify the path in a global 'program-locations' section."
-- TODO: [nice to have] better format control so we can pretty-print the
-- offending part of the project file. Currently the line wrapping breaks any
-- formatting.
renderBadPerPackageCompilerPaths _ = error "renderBadPerPackageCompilerPaths"

-- | The project configuration is not allowed to specify program locations for
-- programs used by the compiler as these have to be the same for each set of
-- packages.
--
-- We cannot check this until we know which programs the compiler uses, which
-- in principle is not until we've configured the compiler.
--
-- Throws 'BadPerPackageCompilerPaths'
checkBadPerPackageCompilerPaths
  :: [ConfiguredProgram]
  -> Map PackageName PackageConfig
  -> IO ()
checkBadPerPackageCompilerPaths compilerPrograms packagesConfig =
  case [ (pkgname, progname)
       | let compProgNames = Set.fromList (map programId compilerPrograms)
       , (pkgname, pkgconf) <- Map.toList packagesConfig
       , progname <- Map.keys (getMapLast (packageConfigProgramPaths pkgconf))
       , progname `Set.member` compProgNames
       ] of
    [] -> return ()
    ps -> throwIO (BadPerPackageCompilerPaths ps)

-- | Filter out non-top-level project configs.
onlyTopLevelProvenance :: Set ProjectConfigProvenance -> Set ProjectConfigProvenance
onlyTopLevelProvenance = Set.filter $ \case
  Implicit -> False
  Explicit ps -> isTopLevelConfigPath ps

-- | The maximum amount of fetch jobs that can run concurrently.
-- For instance, this is used to limit the amount of concurrent downloads from
-- hackage, or the amount of concurrent git clones for
-- source-repository-package stanzas.
maxNumFetchJobs :: Int
maxNumFetchJobs = 2
