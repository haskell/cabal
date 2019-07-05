{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Handling project configuration.
--
module Distribution.Client.ProjectConfig (

    -- * Types for project config
    ProjectConfig(..),
    ProjectConfigBuildOnly(..),
    ProjectConfigShared(..),
    ProjectConfigProvenance(..),
    PackageConfig(..),
    MapLast(..),
    MapMappend(..),

    -- * Project root
    findProjectRoot,
    ProjectRoot(..),
    BadProjectRoot(..),

    -- * Project config files
    readProjectConfig,
    readGlobalConfig,
    readProjectLocalFreezeConfig,
    withProjectOrGlobalConfig,
    writeProjectLocalExtraConfig,
    writeProjectLocalFreezeConfig,
    writeProjectConfigFile,
    commandLineFlagsToProjectConfig,

    -- * Packages within projects
    ProjectPackageLocation(..),
    BadPackageLocations(..),
    BadPackageLocation(..),
    BadPackageLocationMatch(..),
    findProjectPackages,
    fetchAndReadSourcePackages,

    -- * Resolving configuration
    lookupLocalPackageConfig,
    projectConfigWithBuilderRepoContext,
    projectConfigWithSolverRepoContext,
    SolverSettings(..),
    resolveSolverSettings,
    BuildTimeSettings(..),
    resolveBuildTimeSettings,

    -- * Checking configuration
    checkBadPerPackageCompilerPaths,
    BadPerPackageCompilerPaths(..)
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.ProjectConfig.Types
import Distribution.Client.ProjectConfig.Legacy
import Distribution.Client.RebuildMonad
import Distribution.Client.Glob
         ( isTrivialFilePathGlob )
import Distribution.Client.VCS
         ( validateSourceRepos, SourceRepoProblem(..)
         , VCS(..), knownVCSs, configureVCS, syncSourceRepos )

import Distribution.Client.Types
import Distribution.Client.DistDirLayout
         ( DistDirLayout(..), CabalDirLayout(..), ProjectRoot(..) )
import Distribution.Client.GlobalFlags
         ( RepoContext(..), withRepoContext' )
import Distribution.Client.BuildReports.Types
         ( ReportLevel(..) )
import Distribution.Client.Config
         ( loadConfig, getConfigFilePath )
import Distribution.Client.HttpUtils
         ( HttpTransport, configureTransport, transportCheckHttps
         , downloadURI )
import Distribution.Client.Utils.Parsec (renderParseError)

import Distribution.Solver.Types.SourcePackage
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.PackageConstraint
         ( PackageProperty(..) )

import Distribution.Package
         ( PackageName, PackageId, packageId, UnitId )
import Distribution.Types.PackageVersionConstraint
         ( PackageVersionConstraint(..) )
import Distribution.System
         ( Platform )
import Distribution.Types.GenericPackageDescription
         ( GenericPackageDescription )
import Distribution.PackageDescription.Parsec
         ( parseGenericPackageDescription )
import Distribution.Fields
         ( runParseResult, PError, PWarning, showPWarning)
import Distribution.Pretty ()
import Distribution.Types.SourceRepo
         ( SourceRepo(..), RepoType(..), )
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo )
import Distribution.Simple.Program
         ( ConfiguredProgram(..) )
import Distribution.Simple.Setup
         ( Flag(Flag), toFlag, flagToMaybe, flagToList
         , fromFlag, fromFlagOrDefault )
import Distribution.Client.Setup
         ( defaultSolver, defaultMaxBackjumps )
import Distribution.Simple.InstallDirs
         ( PathTemplate, fromPathTemplate
         , toPathTemplate, substPathTemplate, initialPathTemplateEnv )
import Distribution.Simple.Utils
         ( die', warn, notice, info, createDirectoryIfMissingVerbose )
import Distribution.Client.Utils
         ( determineNumJobs )
import Distribution.Utils.NubList
         ( fromNubList )
import Distribution.Verbosity
         ( Verbosity, modifyVerbosity, verbose )
import Distribution.Version
         ( Version )
import Distribution.Deprecated.Text
import qualified Distribution.Deprecated.ParseUtils as OldParser
         ( ParseResult(..), locatedErrorMsg, showPWarning )

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Distribution.Client.Tar as Tar
import qualified Distribution.Client.GZipUtils as GZipUtils

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Exception
import Data.Either
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Hashable as Hashable
import Numeric (showHex)

import System.FilePath hiding (combine)
import System.IO
         ( withBinaryFile, IOMode(ReadMode) )
import System.Directory
import Network.URI
         ( URI(..), URIAuth(..), parseAbsoluteURI, uriToString )


----------------------------------------
-- Resolving configuration to settings
--

-- | Look up a 'PackageConfig' field in the 'ProjectConfig' for a specific
-- 'PackageName'. This returns the configuration that applies to all local
-- packages plus any package-specific configuration for this package.
--
lookupLocalPackageConfig
  :: (Semigroup a, Monoid a)
  => (PackageConfig -> a) -> ProjectConfig -> PackageName
  -> a
lookupLocalPackageConfig field ProjectConfig {
                           projectConfigLocalPackages,
                           projectConfigSpecificPackage
                         } pkgname =
    field projectConfigLocalPackages
 <> maybe mempty field
          (Map.lookup pkgname (getMapMappend projectConfigSpecificPackage))


-- | Use a 'RepoContext' based on the 'BuildTimeSettings'.
--
projectConfigWithBuilderRepoContext :: Verbosity
                                    -> BuildTimeSettings
                                    -> (RepoContext -> IO a) -> IO a
projectConfigWithBuilderRepoContext verbosity BuildTimeSettings{..} =
    withRepoContext'
      verbosity
      buildSettingRemoteRepos
      buildSettingLocalRepos
      buildSettingCacheDir
      buildSettingHttpTransport
      (Just buildSettingIgnoreExpiry)
      buildSettingProgPathExtra


-- | Use a 'RepoContext', but only for the solver. The solver does not use the
-- full facilities of the 'RepoContext' so we can get away with making one
-- that doesn't have an http transport. And that avoids having to have access
-- to the 'BuildTimeSettings'
--
projectConfigWithSolverRepoContext
  :: Verbosity -> ProjectConfigShared -> ProjectConfigBuildOnly
  -> (RepoContext -> IO a)
  -> IO a
projectConfigWithSolverRepoContext verbosity
                                   ProjectConfigShared{..}
                                   ProjectConfigBuildOnly{..} =
    withRepoContext'
      verbosity
      (fromNubList projectConfigRemoteRepos)
      (fromNubList projectConfigLocalRepos)
      (fromFlagOrDefault
                   (error
                    "projectConfigWithSolverRepoContext: projectConfigCacheDir")
                   projectConfigCacheDir)
      (flagToMaybe projectConfigHttpTransport)
      (flagToMaybe projectConfigIgnoreExpiry)
      (fromNubList projectConfigProgPathExtra)


-- | Resolve the project configuration, with all its optional fields, into
-- 'SolverSettings' with no optional fields (by applying defaults).
--
resolveSolverSettings :: ProjectConfig -> SolverSettings
resolveSolverSettings ProjectConfig{
                        projectConfigShared,
                        projectConfigLocalPackages,
                        projectConfigSpecificPackage
                      } =
    SolverSettings {..}
  where
    --TODO: [required eventually] some of these settings need validation, e.g.
    -- the flag assignments need checking.
    solverSettingRemoteRepos       = fromNubList projectConfigRemoteRepos
    solverSettingLocalRepos        = fromNubList projectConfigLocalRepos
    solverSettingConstraints       = projectConfigConstraints
    solverSettingPreferences       = projectConfigPreferences
    solverSettingFlagAssignment    = packageConfigFlagAssignment projectConfigLocalPackages
    solverSettingFlagAssignments   = fmap packageConfigFlagAssignment
                                          (getMapMappend projectConfigSpecificPackage)
    solverSettingCabalVersion      = flagToMaybe projectConfigCabalVersion
    solverSettingSolver            = fromFlag projectConfigSolver
    solverSettingAllowOlder        = fromMaybe mempty projectConfigAllowOlder
    solverSettingAllowNewer        = fromMaybe mempty projectConfigAllowNewer
    solverSettingMaxBackjumps      = case fromFlag projectConfigMaxBackjumps of
                                       n | n < 0     -> Nothing
                                         | otherwise -> Just n
    solverSettingReorderGoals      = fromFlag projectConfigReorderGoals
    solverSettingCountConflicts    = fromFlag projectConfigCountConflicts
    solverSettingMinimizeConflictSet = fromFlag projectConfigMinimizeConflictSet
    solverSettingStrongFlags       = fromFlag projectConfigStrongFlags
    solverSettingAllowBootLibInstalls = fromFlag projectConfigAllowBootLibInstalls
    solverSettingOnlyConstrained   = fromFlag projectConfigOnlyConstrained
    solverSettingIndexState        = flagToMaybe projectConfigIndexState
    solverSettingIndependentGoals  = fromFlag projectConfigIndependentGoals
  --solverSettingShadowPkgs        = fromFlag projectConfigShadowPkgs
  --solverSettingReinstall         = fromFlag projectConfigReinstall
  --solverSettingAvoidReinstalls   = fromFlag projectConfigAvoidReinstalls
  --solverSettingOverrideReinstall = fromFlag projectConfigOverrideReinstall
  --solverSettingUpgradeDeps       = fromFlag projectConfigUpgradeDeps

    ProjectConfigShared {..} = defaults <> projectConfigShared

    defaults = mempty {
       projectConfigSolver            = Flag defaultSolver,
       projectConfigAllowOlder        = Just (AllowOlder mempty),
       projectConfigAllowNewer        = Just (AllowNewer mempty),
       projectConfigMaxBackjumps      = Flag defaultMaxBackjumps,
       projectConfigReorderGoals      = Flag (ReorderGoals False),
       projectConfigCountConflicts    = Flag (CountConflicts True),
       projectConfigMinimizeConflictSet = Flag (MinimizeConflictSet False),
       projectConfigStrongFlags       = Flag (StrongFlags False),
       projectConfigAllowBootLibInstalls = Flag (AllowBootLibInstalls False),
       projectConfigOnlyConstrained   = Flag OnlyConstrainedNone,
       projectConfigIndependentGoals  = Flag (IndependentGoals False)
     --projectConfigShadowPkgs        = Flag False,
     --projectConfigReinstall         = Flag False,
     --projectConfigAvoidReinstalls   = Flag False,
     --projectConfigOverrideReinstall = Flag False,
     --projectConfigUpgradeDeps       = Flag False
    }


-- | Resolve the project configuration, with all its optional fields, into
-- 'BuildTimeSettings' with no optional fields (by applying defaults).
--
resolveBuildTimeSettings :: Verbosity
                         -> CabalDirLayout
                         -> ProjectConfig
                         -> BuildTimeSettings
resolveBuildTimeSettings verbosity
                         CabalDirLayout {
                           cabalLogsDirectory
                         }
                         ProjectConfig {
                           projectConfigShared = ProjectConfigShared {
                             projectConfigRemoteRepos,
                             projectConfigLocalRepos,
                             projectConfigProgPathExtra
                           },
                           projectConfigBuildOnly
                         } =
    BuildTimeSettings {..}
  where
    buildSettingDryRun        = fromFlag    projectConfigDryRun
    buildSettingOnlyDeps      = fromFlag    projectConfigOnlyDeps
    buildSettingSummaryFile   = fromNubList projectConfigSummaryFile
    --buildSettingLogFile       -- defined below, more complicated
    --buildSettingLogVerbosity  -- defined below, more complicated
    buildSettingBuildReports  = fromFlag    projectConfigBuildReports
    buildSettingSymlinkBinDir = flagToList  projectConfigSymlinkBinDir
    buildSettingOneShot       = fromFlag    projectConfigOneShot
    buildSettingNumJobs       = determineNumJobs projectConfigNumJobs
    buildSettingKeepGoing     = fromFlag    projectConfigKeepGoing
    buildSettingOfflineMode   = fromFlag    projectConfigOfflineMode
    buildSettingKeepTempFiles = fromFlag    projectConfigKeepTempFiles
    buildSettingRemoteRepos   = fromNubList projectConfigRemoteRepos
    buildSettingLocalRepos    = fromNubList projectConfigLocalRepos
    buildSettingCacheDir      = fromFlag    projectConfigCacheDir
    buildSettingHttpTransport = flagToMaybe projectConfigHttpTransport
    buildSettingIgnoreExpiry  = fromFlag    projectConfigIgnoreExpiry
    buildSettingReportPlanningFailure
                              = fromFlag projectConfigReportPlanningFailure
    buildSettingProgPathExtra = fromNubList projectConfigProgPathExtra

    ProjectConfigBuildOnly{..} = defaults
                              <> projectConfigBuildOnly

    defaults = mempty {
      projectConfigDryRun                = toFlag False,
      projectConfigOnlyDeps              = toFlag False,
      projectConfigBuildReports          = toFlag NoReports,
      projectConfigReportPlanningFailure = toFlag False,
      projectConfigKeepGoing             = toFlag False,
      projectConfigOneShot               = toFlag False,
      projectConfigOfflineMode           = toFlag False,
      projectConfigKeepTempFiles         = toFlag False,
      projectConfigIgnoreExpiry          = toFlag False
    }

    -- The logging logic: what log file to use and what verbosity.
    --
    -- If the user has specified --remote-build-reporting=detailed, use the
    -- default log file location. If the --build-log option is set, use the
    -- provided location. Otherwise don't use logging, unless building in
    -- parallel (in which case the default location is used).
    --
    buildSettingLogFile :: Maybe (Compiler -> Platform
                               -> PackageId -> UnitId -> FilePath)
    buildSettingLogFile
      | useDefaultTemplate = Just (substLogFileName defaultTemplate)
      | otherwise          = fmap  substLogFileName givenTemplate

    defaultTemplate = toPathTemplate $
                        cabalLogsDirectory </>
                        "$compiler" </> "$libname" <.> "log"
    givenTemplate   = flagToMaybe projectConfigLogFile

    useDefaultTemplate
      | buildSettingBuildReports == DetailedReports = True
      | isJust givenTemplate                        = False
      | isParallelBuild                             = True
      | otherwise                                   = False

    isParallelBuild = buildSettingNumJobs >= 2

    substLogFileName :: PathTemplate
                     -> Compiler -> Platform
                     -> PackageId -> UnitId -> FilePath
    substLogFileName template compiler platform pkgid uid =
        fromPathTemplate (substPathTemplate env template)
      where
        env = initialPathTemplateEnv
                pkgid uid (compilerInfo compiler) platform

    -- If the user has specified --remote-build-reporting=detailed or
    -- --build-log, use more verbose logging.
    --
    buildSettingLogVerbosity
      | overrideVerbosity = modifyVerbosity (max verbose) verbosity
      | otherwise         = verbosity

    overrideVerbosity
      | buildSettingBuildReports == DetailedReports = True
      | isJust givenTemplate                        = True
      | isParallelBuild                             = False
      | otherwise                                   = False


---------------------------------------------
-- Reading and writing project config files
--

-- | Find the root of this project.
--
-- Searches for an explicit @cabal.project@ file, in the current directory or
-- parent directories. If no project file is found then the current dir is the
-- project root (and the project will use an implicit config).
--
findProjectRoot :: Maybe FilePath -- ^ starting directory, or current directory
                -> Maybe FilePath -- ^ @cabal.project@ file name override
                -> IO (Either BadProjectRoot ProjectRoot)
findProjectRoot _ (Just projectFile)
  | isAbsolute projectFile = do
    exists <- doesFileExist projectFile
    if exists
      then do projectFile' <- canonicalizePath projectFile
              let projectRoot = ProjectRootExplicit (takeDirectory projectFile')
                                                    (takeFileName projectFile')
              return (Right projectRoot)
      else return (Left (BadProjectRootExplicitFile projectFile))

findProjectRoot mstartdir mprojectFile = do
    startdir <- maybe getCurrentDirectory canonicalizePath mstartdir
    homedir  <- getHomeDirectory
    probe startdir homedir
  where
    projectFileName = fromMaybe "cabal.project" mprojectFile

    -- Search upwards. If we get to the users home dir or the filesystem root,
    -- then use the current dir
    probe startdir homedir = go startdir
      where
        go dir | isDrive dir || dir == homedir =
          case mprojectFile of
            Nothing   -> return (Right (ProjectRootImplicit startdir))
            Just file -> return (Left (BadProjectRootExplicitFile file))
        go dir = do
          exists <- doesFileExist (dir </> projectFileName)
          if exists
            then return (Right (ProjectRootExplicit dir projectFileName))
            else go (takeDirectory dir)

   --TODO: [nice to have] add compat support for old style sandboxes


-- | Errors returned by 'findProjectRoot'.
--
data BadProjectRoot = BadProjectRootExplicitFile FilePath
#if MIN_VERSION_base(4,8,0)
  deriving (Show, Typeable)
#else
  deriving (Typeable)

instance Show BadProjectRoot where
  show = renderBadProjectRoot
#endif

instance Exception BadProjectRoot where
#if MIN_VERSION_base(4,8,0)
  displayException = renderBadProjectRoot
#endif

renderBadProjectRoot :: BadProjectRoot -> String
renderBadProjectRoot (BadProjectRootExplicitFile projectFile) =
    "The given project file '" ++ projectFile ++ "' does not exist."

withProjectOrGlobalConfig :: Verbosity
                          -> Flag FilePath
                          -> IO a
                          -> (ProjectConfig -> IO a)
                          -> IO a
withProjectOrGlobalConfig verbosity globalConfigFlag with without = do
  globalConfig <- runRebuild "" $ readGlobalConfig verbosity globalConfigFlag

  let
    res' = catch with
      $ \case
        (BadPackageLocations prov locs)
          | prov == Set.singleton Implicit
          , let
            isGlobErr (BadLocGlobEmptyMatch _) = True
            isGlobErr _ = False
          , any isGlobErr locs ->
            without globalConfig
        err -> throwIO err

  catch res'
    $ \case
      (BadProjectRootExplicitFile "") -> without globalConfig
      err -> throwIO err

-- | Read all the config relevant for a project. This includes the project
-- file if any, plus other global config.
--
readProjectConfig :: Verbosity
                  -> Flag FilePath
                  -> DistDirLayout
                  -> Rebuild ProjectConfig
readProjectConfig verbosity configFileFlag distDirLayout = do
    global <- readGlobalConfig                verbosity configFileFlag
    local  <- readProjectLocalConfigOrDefault verbosity distDirLayout
    freeze <- readProjectLocalFreezeConfig    verbosity distDirLayout
    extra  <- readProjectLocalExtraConfig     verbosity distDirLayout
    return (global <> local <> freeze <> extra)


-- | Reads an explicit @cabal.project@ file in the given project root dir,
-- or returns the default project config for an implicitly defined project.
--
readProjectLocalConfigOrDefault :: Verbosity
                                -> DistDirLayout
                                -> Rebuild ProjectConfig
readProjectLocalConfigOrDefault verbosity distDirLayout = do
  usesExplicitProjectRoot <- liftIO $ doesFileExist projectFile
  if usesExplicitProjectRoot
    then do
      readProjectFile verbosity distDirLayout "" "project file"
    else do
      monitorFiles [monitorNonExistentFile projectFile]
      return defaultImplicitProjectConfig

  where
    projectFile = distProjectFile distDirLayout ""

    defaultImplicitProjectConfig :: ProjectConfig
    defaultImplicitProjectConfig =
      mempty {
        -- We expect a package in the current directory.
        projectPackages         = [ "./*.cabal" ],

        -- This is to automatically pick up deps that we unpack locally.
        projectPackagesOptional = [ "./*/*.cabal" ],

        projectConfigProvenance = Set.singleton Implicit
      }

-- | Reads a @cabal.project.local@ file in the given project root dir,
-- or returns empty. This file gets written by @cabal configure@, or in
-- principle can be edited manually or by other tools.
--
readProjectLocalExtraConfig :: Verbosity -> DistDirLayout
                            -> Rebuild ProjectConfig
readProjectLocalExtraConfig verbosity distDirLayout =
    readProjectFile verbosity distDirLayout "local"
                             "project local configuration file"

-- | Reads a @cabal.project.freeze@ file in the given project root dir,
-- or returns empty. This file gets written by @cabal freeze@, or in
-- principle can be edited manually or by other tools.
--
readProjectLocalFreezeConfig :: Verbosity -> DistDirLayout
                             -> Rebuild ProjectConfig
readProjectLocalFreezeConfig verbosity distDirLayout =
    readProjectFile verbosity distDirLayout "freeze"
                             "project freeze file"

-- | Reads a named config file in the given project root dir, or returns empty.
--
readProjectFile :: Verbosity
                -> DistDirLayout
                -> String
                -> String
                -> Rebuild ProjectConfig
readProjectFile verbosity DistDirLayout{distProjectFile}
                         extensionName extensionDescription = do
    exists <- liftIO $ doesFileExist extensionFile
    if exists
      then do monitorFiles [monitorFileHashed extensionFile]
              addProjectFileProvenance <$> liftIO readExtensionFile
      else do monitorFiles [monitorNonExistentFile extensionFile]
              return mempty
  where
    extensionFile = distProjectFile extensionName

    readExtensionFile =
          reportParseResult verbosity extensionDescription extensionFile
        . parseProjectConfig
      =<< readFile extensionFile

    addProjectFileProvenance config =
      config {
        projectConfigProvenance =
          Set.insert (Explicit extensionFile) (projectConfigProvenance config)
      }


-- | Parse the 'ProjectConfig' format.
--
-- For the moment this is implemented in terms of parsers for legacy
-- configuration types, plus a conversion.
--
parseProjectConfig :: String -> OldParser.ParseResult ProjectConfig
parseProjectConfig content =
    convertLegacyProjectConfig <$>
      parseLegacyProjectConfig content


-- | Render the 'ProjectConfig' format.
--
-- For the moment this is implemented in terms of a pretty printer for the
-- legacy configuration types, plus a conversion.
--
showProjectConfig :: ProjectConfig -> String
showProjectConfig =
    showLegacyProjectConfig . convertToLegacyProjectConfig


-- | Write a @cabal.project.local@ file in the given project root dir.
--
writeProjectLocalExtraConfig :: DistDirLayout -> ProjectConfig -> IO ()
writeProjectLocalExtraConfig DistDirLayout{distProjectFile} =
    writeProjectConfigFile (distProjectFile "local")


-- | Write a @cabal.project.freeze@ file in the given project root dir.
--
writeProjectLocalFreezeConfig :: DistDirLayout -> ProjectConfig -> IO ()
writeProjectLocalFreezeConfig DistDirLayout{distProjectFile} =
    writeProjectConfigFile (distProjectFile "freeze")


-- | Write in the @cabal.project@ format to the given file.
--
writeProjectConfigFile :: FilePath -> ProjectConfig -> IO ()
writeProjectConfigFile file =
    writeFile file . showProjectConfig


-- | Read the user's @~/.cabal/config@ file.
--
readGlobalConfig :: Verbosity -> Flag FilePath -> Rebuild ProjectConfig
readGlobalConfig verbosity configFileFlag = do
    config     <- liftIO (loadConfig verbosity configFileFlag)
    configFile <- liftIO (getConfigFilePath configFileFlag)
    monitorFiles [monitorFileHashed configFile]
    return (convertLegacyGlobalConfig config)

reportParseResult :: Verbosity -> String -> FilePath -> OldParser.ParseResult a -> IO a
reportParseResult verbosity _filetype filename (OldParser.ParseOk warnings x) = do
    unless (null warnings) $
      let msg = unlines (map (OldParser.showPWarning filename) warnings)
       in warn verbosity msg
    return x
reportParseResult verbosity filetype filename (OldParser.ParseFailed err) =
    let (line, msg) = OldParser.locatedErrorMsg err
     in die' verbosity $ "Error parsing " ++ filetype ++ " " ++ filename
           ++ maybe "" (\n -> ':' : show n) line ++ ":\n" ++ msg


---------------------------------------------
-- Finding packages in the project
--

-- | The location of a package as part of a project. Local file paths are
-- either absolute (if the user specified it as such) or they are relative
-- to the project root.
--
data ProjectPackageLocation =
     ProjectPackageLocalCabalFile FilePath
   | ProjectPackageLocalDirectory FilePath FilePath -- dir and .cabal file
   | ProjectPackageLocalTarball   FilePath
   | ProjectPackageRemoteTarball  URI
   | ProjectPackageRemoteRepo     SourceRepo
   | ProjectPackageNamed          PackageVersionConstraint
  deriving Show


-- | Exception thrown by 'findProjectPackages'.
--
data BadPackageLocations
   = BadPackageLocations (Set ProjectConfigProvenance) [BadPackageLocation]
#if MIN_VERSION_base(4,8,0)
  deriving (Show, Typeable)
#else
  deriving (Typeable)

instance Show BadPackageLocations where
  show = renderBadPackageLocations
#endif

instance Exception BadPackageLocations where
#if MIN_VERSION_base(4,8,0)
  displayException = renderBadPackageLocations
#endif
--TODO: [nice to have] custom exception subclass for Doc rendering, colour etc

data BadPackageLocation
   = BadPackageLocationFile    BadPackageLocationMatch
   | BadLocGlobEmptyMatch      String
   | BadLocGlobBadMatches      String [BadPackageLocationMatch]
   | BadLocUnexpectedUriScheme String
   | BadLocUnrecognisedUri     String
   | BadLocUnrecognised        String
  deriving Show

data BadPackageLocationMatch
   = BadLocUnexpectedFile      String
   | BadLocNonexistantFile     String
   | BadLocDirNoCabalFile      String
   | BadLocDirManyCabalFiles   String
  deriving Show

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
           "When using configuration(s) from "
        ++ intercalate ", " (mapMaybe getExplicit (Set.toList provenance))
        ++ ", the following errors occurred:\n"
        ++ renderErrors renderBadPackageLocation

    getExplicit (Explicit path) = Just path
    getExplicit Implicit        = Nothing

--TODO: [nice to have] keep track of the config file (and src loc) packages
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
     ++ pkglocstr ++ "' was found.\n"
     ++ "Please create a package description file <pkgname>.cabal "
     ++ "or a cabal.project file referencing the packages you "
     ++ "want to build."
    _ -> renderBadPackageLocation bpl

renderBadPackageLocation :: BadPackageLocation -> String
renderBadPackageLocation bpl = case bpl of
    BadPackageLocationFile badmatch ->
        renderBadPackageLocationMatch badmatch
    BadLocGlobEmptyMatch pkglocstr ->
        "The package location glob '" ++ pkglocstr
     ++ "' does not match any files or directories."
    BadLocGlobBadMatches pkglocstr failures ->
        "The package location glob '" ++ pkglocstr ++ "' does not match any "
     ++ "recognised forms of package. "
     ++ concatMap ((' ':) . renderBadPackageLocationMatch) failures
    BadLocUnexpectedUriScheme pkglocstr ->
        "The package location URI '" ++ pkglocstr ++ "' does not use a "
     ++ "supported URI scheme. The supported URI schemes are http, https and "
     ++ "file."
    BadLocUnrecognisedUri pkglocstr ->
        "The package location URI '" ++ pkglocstr ++ "' does not appear to "
     ++ "be a valid absolute URI."
    BadLocUnrecognised pkglocstr ->
        "The package location syntax '" ++ pkglocstr ++ "' is not recognised."

renderBadPackageLocationMatch :: BadPackageLocationMatch -> String
renderBadPackageLocationMatch bplm = case bplm of
    BadLocUnexpectedFile pkglocstr ->
        "The package location '" ++ pkglocstr ++ "' is not recognised. The "
     ++ "supported file targets are .cabal files, .tar.gz tarballs or package "
     ++ "directories (i.e. directories containing a .cabal file)."
    BadLocNonexistantFile pkglocstr ->
        "The package location '" ++ pkglocstr ++ "' does not exist."
    BadLocDirNoCabalFile pkglocstr ->
        "The package directory '" ++ pkglocstr ++ "' does not contain any "
     ++ ".cabal file."
    BadLocDirManyCabalFiles pkglocstr ->
        "The package directory '" ++ pkglocstr ++ "' contains multiple "
     ++ ".cabal files (which is not currently supported)."

-- | Given the project config,
--
-- Throws 'BadPackageLocations'.
--
findProjectPackages :: DistDirLayout -> ProjectConfig
                    -> Rebuild [ProjectPackageLocation]
findProjectPackages DistDirLayout{distProjectRootDirectory}
                    ProjectConfig{..} = do

    requiredPkgs <- findPackageLocations True    projectPackages
    optionalPkgs <- findPackageLocations False   projectPackagesOptional
    let repoPkgs  = map ProjectPackageRemoteRepo projectPackagesRepo
        namedPkgs = map ProjectPackageNamed      projectPackagesNamed

    return (concat [requiredPkgs, optionalPkgs, repoPkgs, namedPkgs])
  where
    findPackageLocations required pkglocstr = do
      (problems, pkglocs) <-
        partitionEithers <$> mapM (findPackageLocation required) pkglocstr
      unless (null problems) $
        liftIO $ throwIO $ BadPackageLocations projectConfigProvenance problems
      return (concat pkglocs)


    findPackageLocation :: Bool -> String
                        -> Rebuild (Either BadPackageLocation
                                          [ProjectPackageLocation])
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
        Nothing              -> return (Left (BadLocUnrecognised pkglocstr))
        Just (Left _)        -> return (Right []) -- it's optional
        Just (Right pkglocs) -> return (Right pkglocs)


    checkIsUriPackage, checkIsFileGlobPackage, checkIsSingleFilePackage
      :: String -> Rebuild (Maybe (Either BadPackageLocation
                                         [ProjectPackageLocation]))
    checkIsUriPackage pkglocstr =
      case parseAbsoluteURI pkglocstr of
        Just uri@URI {
            uriScheme    = scheme,
            uriAuthority = Just URIAuth { uriRegName = host },
            uriPath      = path,
            uriQuery     = query,
            uriFragment  = frag
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
            recognisedScheme = scheme == "http:" || scheme == "https:"
                            || scheme == "file:"

        _ -> return Nothing


    checkIsFileGlobPackage pkglocstr =
      case simpleParse pkglocstr of
        Nothing   -> return Nothing
        Just glob -> liftM Just $ do
          matches <- matchFileGlob glob
          case matches of
            [] | isJust (isTrivialFilePathGlob glob)
               -> return (Left (BadPackageLocationFile
                                  (BadLocNonexistantFile pkglocstr)))

            [] -> return (Left (BadLocGlobEmptyMatch pkglocstr))

            _  -> do
              (failures, pkglocs) <- partitionEithers <$>
                                     mapM checkFilePackageMatch matches
              return $! case (failures, pkglocs) of
                ([failure], []) | isJust (isTrivialFilePathGlob glob)
                        -> Left (BadPackageLocationFile failure)
                (_, []) -> Left (BadLocGlobBadMatches pkglocstr failures)
                _       -> Right pkglocs


    checkIsSingleFilePackage pkglocstr = do
      let filename = distProjectRootDirectory </> pkglocstr
      isFile <- liftIO $ doesFileExist filename
      isDir  <- liftIO $ doesDirectoryExist filename
      if isFile || isDir
        then checkFilePackageMatch pkglocstr
         >>= either (return . Just . Left  . BadPackageLocationFile)
                    (return . Just . Right . (\x->[x]))
        else return Nothing


    checkFilePackageMatch :: String -> Rebuild (Either BadPackageLocationMatch
                                                       ProjectPackageLocation)
    checkFilePackageMatch pkglocstr = do
      -- The pkglocstr may be absolute or may be relative to the project root.
      -- Either way, </> does the right thing here. We return relative paths if
      -- they were relative in the first place.
      let abspath = distProjectRootDirectory </> pkglocstr
      isFile <- liftIO $ doesFileExist abspath
      isDir  <- liftIO $ doesDirectoryExist abspath
      parentDirExists <- case takeDirectory abspath of
                           []  -> return False
                           dir -> liftIO $ doesDirectoryExist dir
      case () of
        _ | isDir
         -> do matches <- matchFileGlob (globStarDotCabal pkglocstr)
               case matches of
                 [cabalFile]
                     -> return (Right (ProjectPackageLocalDirectory
                                         pkglocstr cabalFile))
                 []  -> return (Left (BadLocDirNoCabalFile pkglocstr))
                 _   -> return (Left (BadLocDirManyCabalFiles pkglocstr))

          | extensionIsTarGz pkglocstr
         -> return (Right (ProjectPackageLocalTarball pkglocstr))

          | takeExtension pkglocstr == ".cabal"
         -> return (Right (ProjectPackageLocalCabalFile pkglocstr))

          | isFile
         -> return (Left (BadLocUnexpectedFile pkglocstr))

          | parentDirExists
         -> return (Left (BadLocNonexistantFile pkglocstr))

          | otherwise
         -> return (Left (BadLocUnexpectedFile pkglocstr))


    extensionIsTarGz f = takeExtension f                 == ".gz"
                      && takeExtension (dropExtension f) == ".tar"


-- | A glob to find all the cabal files in a directory.
--
-- For a directory @some/dir/@, this is a glob of the form @some/dir/\*.cabal@.
-- The directory part can be either absolute or relative.
--
globStarDotCabal :: FilePath -> FilePathGlob
globStarDotCabal dir =
    FilePathGlob
      (if isAbsolute dir then FilePathRoot root else FilePathRelative)
      (foldr (\d -> GlobDir [Literal d])
             (GlobFile [WildCard, Literal ".cabal"]) dirComponents)
  where
    (root, dirComponents) = fmap splitDirectories (splitDrive dir)


--TODO: [code cleanup] use sufficiently recent transformers package
mplusMaybeT :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
mplusMaybeT ma mb = do
  mx <- ma
  case mx of
    Nothing -> mb
    Just x  -> return (Just x)


-------------------------------------------------
-- Fetching and reading packages in the project
--

-- | Read the @.cabal@ files for a set of packages. For remote tarballs and
-- VCS source repos this also fetches them if needed.
--
-- Note here is where we convert from project-root relative paths to absolute
-- paths.
--
fetchAndReadSourcePackages
  :: Verbosity
  -> DistDirLayout
  -> ProjectConfigShared
  -> ProjectConfigBuildOnly
  -> [ProjectPackageLocation]
  -> Rebuild [PackageSpecifier (SourcePackage UnresolvedPkgLoc)]
fetchAndReadSourcePackages verbosity distDirLayout
                           projectConfigShared
                           projectConfigBuildOnly
                           pkgLocations = do

    pkgsLocalDirectory <-
      sequence
        [ readSourcePackageLocalDirectory verbosity dir cabalFile
        | location <- pkgLocations
        , (dir, cabalFile) <- projectPackageLocal location ]

    pkgsLocalTarball <-
      sequence
        [ readSourcePackageLocalTarball verbosity path
        | ProjectPackageLocalTarball path <- pkgLocations ]

    pkgsRemoteTarball <- do
      getTransport <- delayInitSharedResource $
                      configureTransport verbosity progPathExtra
                                         preferredHttpTransport
      sequence
        [ fetchAndReadSourcePackageRemoteTarball verbosity distDirLayout
                                                 getTransport uri
        | ProjectPackageRemoteTarball uri <- pkgLocations ]

    pkgsRemoteRepo <-
      syncAndReadSourcePackagesRemoteRepos
        verbosity distDirLayout
        projectConfigShared
        [ repo | ProjectPackageRemoteRepo repo <- pkgLocations ]

    let pkgsNamed =
          [ NamedPackage pkgname [PackagePropertyVersion verrange]
          | ProjectPackageNamed (PackageVersionConstraint pkgname verrange) <- pkgLocations ]

    return $ concat
      [ pkgsLocalDirectory
      , pkgsLocalTarball
      , pkgsRemoteTarball
      , pkgsRemoteRepo
      , pkgsNamed
      ]
  where
    projectPackageLocal (ProjectPackageLocalDirectory dir file) = [(dir, file)]
    projectPackageLocal (ProjectPackageLocalCabalFile     file) = [(dir, file)]
                                                where dir = takeDirectory file
    projectPackageLocal _ = []

    progPathExtra = fromNubList (projectConfigProgPathExtra projectConfigShared)
    preferredHttpTransport =
      flagToMaybe (projectConfigHttpTransport projectConfigBuildOnly)

-- | A helper for 'fetchAndReadSourcePackages' to handle the case of
-- 'ProjectPackageLocalDirectory' and 'ProjectPackageLocalCabalFile'.
-- We simply read the @.cabal@ file.
--
readSourcePackageLocalDirectory
  :: Verbosity
  -> FilePath  -- ^ The package directory
  -> FilePath  -- ^ The package @.cabal@ file
  -> Rebuild (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
readSourcePackageLocalDirectory verbosity dir cabalFile = do
    monitorFiles [monitorFileHashed cabalFile]
    root <- askRoot
    let location = LocalUnpackedPackage (root </> dir)
    liftIO $ fmap (mkSpecificSourcePackage location)
           . readSourcePackageCabalFile verbosity cabalFile
         =<< BS.readFile (root </> cabalFile)


-- | A helper for 'fetchAndReadSourcePackages' to handle the case of
-- 'ProjectPackageLocalTarball'. We scan through the @.tar.gz@ file to find
-- the @.cabal@ file and read that.
--
readSourcePackageLocalTarball
  :: Verbosity
  -> FilePath
  -> Rebuild (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
readSourcePackageLocalTarball verbosity tarballFile = do
    monitorFiles [monitorFile tarballFile]
    root <- askRoot
    let location = LocalTarballPackage (root </> tarballFile)
    liftIO $ fmap (mkSpecificSourcePackage location)
           . uncurry (readSourcePackageCabalFile verbosity)
         =<< extractTarballPackageCabalFile (root </> tarballFile)


-- | A helper for 'fetchAndReadSourcePackages' to handle the case of
-- 'ProjectPackageRemoteTarball'. We download the tarball to the dist src dir
-- and after that handle it like the local tarball case.
--
fetchAndReadSourcePackageRemoteTarball
  :: Verbosity
  -> DistDirLayout
  -> Rebuild HttpTransport
  -> URI
  -> Rebuild (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
fetchAndReadSourcePackageRemoteTarball verbosity
                                       DistDirLayout {
                                         distDownloadSrcDirectory
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
        createDirectoryIfMissingVerbose verbosity True
                                        distDownloadSrcDirectory
        _ <- downloadURI transport verbosity tarballUri tarballFile
        return ()

      -- Read
      monitorFiles [monitorFile tarballFile]
      let location = RemoteTarballPackage tarballUri tarballFile
      liftIO $ fmap (mkSpecificSourcePackage location)
             . uncurry (readSourcePackageCabalFile verbosity)
           =<< extractTarballPackageCabalFile tarballFile
  where
    tarballStem = distDownloadSrcDirectory
              </> localFileNameForRemoteTarball tarballUri
    tarballFile = tarballStem <.> "tar.gz"

    monitor :: FileMonitor URI (PackageSpecifier (SourcePackage UnresolvedPkgLoc))
    monitor = newFileMonitor (tarballStem <.> "cache")


-- | A helper for 'fetchAndReadSourcePackages' to handle all the cases of
-- 'ProjectPackageRemoteRepo'.
--
syncAndReadSourcePackagesRemoteRepos
  :: Verbosity
  -> DistDirLayout
  -> ProjectConfigShared
  -> [SourceRepo]
  -> Rebuild [PackageSpecifier (SourcePackage UnresolvedPkgLoc)]
syncAndReadSourcePackagesRemoteRepos verbosity
                                     DistDirLayout{distDownloadSrcDirectory}
                                     ProjectConfigShared {
                                       projectConfigProgPathExtra
                                     }
                                    repos = do

    repos' <- either reportSourceRepoProblems return $
              validateSourceRepos repos

    -- All 'SourceRepo's grouped by referring to the "same" remote repo
    -- instance. So same location but can differ in commit/tag/branch/subdir.
    let reposByLocation :: Map (RepoType, String)
                               [(SourceRepo, RepoType)]
        reposByLocation = Map.fromListWith (++)
                            [ ((rtype, rloc), [(repo, vcsRepoType vcs)])
                            | (repo, rloc, rtype, vcs) <- repos' ]

    --TODO: pass progPathExtra on to 'configureVCS'
    let _progPathExtra = fromNubList projectConfigProgPathExtra
    getConfiguredVCS <- delayInitSharedResources $ \repoType ->
                          let Just vcs = Map.lookup repoType knownVCSs in
                          configureVCS verbosity {-progPathExtra-} vcs

    concat <$> sequence
      [ rerunIfChanged verbosity monitor repoGroup' $ do
          vcs' <- getConfiguredVCS repoType
          syncRepoGroupAndReadSourcePackages vcs' pathStem repoGroup'
      | repoGroup@((primaryRepo, repoType):_) <- Map.elems reposByLocation
      , let repoGroup' = map fst repoGroup
            pathStem = distDownloadSrcDirectory
                   </> localFileNameForRemoteRepo primaryRepo
            monitor :: FileMonitor
                         [SourceRepo]
                         [PackageSpecifier (SourcePackage UnresolvedPkgLoc)]
            monitor  = newFileMonitor (pathStem <.> "cache")
      ]
  where
    syncRepoGroupAndReadSourcePackages
      :: VCS ConfiguredProgram
      -> FilePath
      -> [SourceRepo]
      -> Rebuild [PackageSpecifier (SourcePackage UnresolvedPkgLoc)]
    syncRepoGroupAndReadSourcePackages vcs pathStem repoGroup = do
        liftIO $ createDirectoryIfMissingVerbose verbosity False
                                                 distDownloadSrcDirectory

        -- For syncing we don't care about different 'SourceRepo' values that
        -- are just different subdirs in the same repo.
        syncSourceRepos verbosity vcs
          [ (repo, repoPath)
          | (repo, _, repoPath) <- repoGroupWithPaths ]

        -- But for reading we go through each 'SourceRepo' including its subdir
        -- value and have to know which path each one ended up in.
        sequence
          [ readPackageFromSourceRepo repoWithSubdir repoPath
          | (_, reposWithSubdir, repoPath) <- repoGroupWithPaths
          , repoWithSubdir <- reposWithSubdir ]
      where
        -- So to do both things above, we pair them up here.
        repoGroupWithPaths =
          zipWith (\(x, y) z -> (x,y,z))
                  (Map.toList
                    (Map.fromListWith (++)
                      [ (repo { repoSubdir = Nothing }, [repo])
                      | repo <- repoGroup ]))
                  repoPaths

        -- The repos in a group are given distinct names by simple enumeration
        -- foo, foo-2, foo-3 etc
        repoPaths = pathStem
                  : [ pathStem ++ "-" ++ show (i :: Int) | i <- [2..] ]

    readPackageFromSourceRepo repo repoPath = do
        let packageDir = maybe repoPath (repoPath </>) (repoSubdir repo)
        entries <- liftIO $ getDirectoryContents packageDir
        --TODO: wrap exceptions
        case filter (\e -> takeExtension e == ".cabal") entries of
          []       -> liftIO $ throwIO $ NoCabalFileFound packageDir
          (_:_:_)  -> liftIO $ throwIO $ MultipleCabalFilesFound packageDir
          [cabalFileName] -> do
            monitorFiles [monitorFileHashed cabalFilePath]
            liftIO $ fmap (mkSpecificSourcePackage location)
                   . readSourcePackageCabalFile verbosity cabalFilePath
                 =<< BS.readFile cabalFilePath
            where
              cabalFilePath = packageDir </> cabalFileName
              location      = RemoteSourceRepoPackage repo packageDir


    reportSourceRepoProblems :: [(SourceRepo, SourceRepoProblem)] -> Rebuild a
    reportSourceRepoProblems = liftIO . die' verbosity . renderSourceRepoProblems

    renderSourceRepoProblems :: [(SourceRepo, SourceRepoProblem)] -> String
    renderSourceRepoProblems = unlines . map show -- "TODO: the repo problems"


-- | Utility used by all the helpers of 'fetchAndReadSourcePackages' to make an
-- appropriate @'PackageSpecifier' ('SourcePackage' (..))@ for a given package
-- from a given location.
--
mkSpecificSourcePackage :: PackageLocation FilePath
                        -> GenericPackageDescription
                        -> PackageSpecifier
                             (SourcePackage (PackageLocation (Maybe FilePath)))
mkSpecificSourcePackage location pkg =
    SpecificSourcePackage SourcePackage {
      packageInfoId        = packageId pkg,
      packageDescription   = pkg,
      --TODO: it is silly that we still have to use a Maybe FilePath here
      packageSource        = fmap Just location,
      packageDescrOverride = Nothing
    }


-- | Errors reported upon failing to parse a @.cabal@ file.
--
data CabalFileParseError = CabalFileParseError
    FilePath        -- ^ @.cabal@ file path
    BS.ByteString   -- ^ @.cabal@ file contents
    [PError]        -- ^ errors
    (Maybe Version) -- ^ We might discover the spec version the package needs
    [PWarning]      -- ^ warnings
  deriving (Typeable)

-- | Manual instance which skips file contentes
instance Show CabalFileParseError where
    showsPrec d (CabalFileParseError fp _ es mv ws) = showParen (d > 10)
        $ showString "CabalFileParseError"
        . showChar ' ' . showsPrec 11 fp
        . showChar ' ' . showsPrec 11 ("" :: String)
        . showChar ' ' . showsPrec 11 es
        . showChar ' ' . showsPrec 11 mv
        . showChar ' ' . showsPrec 11 ws

instance Exception CabalFileParseError
#if MIN_VERSION_base(4,8,0)
  where
  displayException = renderCabalFileParseError
#endif

renderCabalFileParseError :: CabalFileParseError -> String
renderCabalFileParseError (CabalFileParseError filePath contents errors _ warnings) =
    renderParseError filePath contents errors warnings

-- | Wrapper for the @.cabal@ file parser. It reports warnings on higher
-- verbosity levels and throws 'CabalFileParseError' on failure.
--
readSourcePackageCabalFile :: Verbosity
                           -> FilePath
                           -> BS.ByteString
                           -> IO GenericPackageDescription
readSourcePackageCabalFile verbosity pkgfilename content =
    case runParseResult (parseGenericPackageDescription content) of
      (warnings, Right pkg) -> do
        unless (null warnings) $
          info verbosity (formatWarnings warnings)
        return pkg

      (warnings, Left (mspecVersion, errors)) ->
        throwIO $ CabalFileParseError pkgfilename content errors mspecVersion warnings
  where
    formatWarnings warnings =
        "The package description file " ++ pkgfilename
     ++ " has warnings: "
     ++ unlines (map (showPWarning pkgfilename) warnings)


-- | When looking for a package's @.cabal@ file we can find none, or several,
-- both of which are failures.
--
data CabalFileSearchFailure
   = NoCabalFileFound FilePath
   | MultipleCabalFilesFound FilePath
  deriving (Show, Typeable)

instance Exception CabalFileSearchFailure


-- | Find the @.cabal@ file within a tarball file and return it by value.
--
-- Can fail with a 'Tar.FormatError' or 'CabalFileSearchFailure' exception.
--
extractTarballPackageCabalFile :: FilePath -> IO (FilePath, BS.ByteString)
extractTarballPackageCabalFile tarballFile =
    withBinaryFile tarballFile ReadMode $ \hnd -> do
      content <- LBS.hGetContents hnd
      case extractTarballPackageCabalFilePure tarballFile content of
        Left (Left  e) -> throwIO e
        Left (Right e) -> throwIO e
        Right (fileName, fileContent) ->
          (,) fileName <$> evaluate (LBS.toStrict fileContent)


-- | Scan through a tar file stream and collect the @.cabal@ file, or fail.
--
extractTarballPackageCabalFilePure :: FilePath
                                   -> LBS.ByteString
                                   -> Either (Either Tar.FormatError
                                                     CabalFileSearchFailure)
                                             (FilePath, LBS.ByteString)
extractTarballPackageCabalFilePure tarballFile =
      check
    . accumEntryMap
    . Tar.filterEntries isCabalFile
    . Tar.read
    . GZipUtils.maybeDecompress
  where
    accumEntryMap = Tar.foldlEntries
                      (\m e -> Map.insert (Tar.entryTarPath e) e m)
                      Map.empty

    check (Left (e, _m)) = Left (Left e)
    check (Right m) = case Map.elems m of
        []     -> Left (Right $ NoCabalFileFound tarballFile)
        [file] -> case Tar.entryContent file of
          Tar.NormalFile content _ -> Right (Tar.entryPath file, content)
          _                        -> Left (Right $ NoCabalFileFound tarballFile)
        _files -> Left (Right $ MultipleCabalFilesFound tarballFile)

    isCabalFile e = case splitPath (Tar.entryPath e) of
      [     _dir, file] -> takeExtension file == ".cabal"
      [".", _dir, file] -> takeExtension file == ".cabal"
      _                 -> False


-- | The name to use for a local file for a remote tarball 'SourceRepo'.
-- This is deterministic based on the remote tarball URI, and is intended
-- to produce non-clashing file names for different tarballs.
--
localFileNameForRemoteTarball :: URI -> FilePath
localFileNameForRemoteTarball uri =
    mangleName uri
 ++ "-" ++  showHex locationHash ""
  where
    mangleName = truncateString 10 . dropExtension . dropExtension
               . takeFileName . dropTrailingPathSeparator . uriPath

    locationHash :: Word
    locationHash = fromIntegral (Hashable.hash (uriToString id uri ""))


-- | The name to use for a local file or dir for a remote 'SourceRepo'.
-- This is deterministic based on the source repo identity details, and
-- intended to produce non-clashing file names for different repos.
--
localFileNameForRemoteRepo :: SourceRepo -> FilePath
localFileNameForRemoteRepo SourceRepo{repoType, repoLocation, repoModule} =
    maybe "" ((++ "-") . mangleName) repoLocation
 ++ showHex locationHash ""
  where
    mangleName = truncateString 10 . dropExtension
               . takeFileName . dropTrailingPathSeparator

    -- just the parts that make up the "identity" of the repo
    locationHash :: Word
    locationHash =
      fromIntegral (Hashable.hash (show repoType, repoLocation, repoModule))


-- | Truncate a string, with a visual indication that it is truncated.
truncateString :: Int -> String -> String
truncateString n s | length s <= n = s
                   | otherwise     = take (n-1) s ++ "_"


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
#if MIN_VERSION_base(4,8,0)
  deriving (Show, Typeable)
#else
  deriving (Typeable)

instance Show BadPerPackageCompilerPaths where
  show = renderBadPerPackageCompilerPaths
#endif

instance Exception BadPerPackageCompilerPaths where
#if MIN_VERSION_base(4,8,0)
  displayException = renderBadPerPackageCompilerPaths
#endif
--TODO: [nice to have] custom exception subclass for Doc rendering, colour etc

renderBadPerPackageCompilerPaths :: BadPerPackageCompilerPaths -> String
renderBadPerPackageCompilerPaths
  (BadPerPackageCompilerPaths ((pkgname, progname) : _)) =
    "The path to the compiler program (or programs used by the compiler) "
 ++ "cannot be specified on a per-package basis in the cabal.project file "
 ++ "(i.e. setting the '" ++ progname ++ "-location' for package '"
 ++ display pkgname ++ "'). All packages have to use the same compiler, so "
 ++ "specify the path in a global 'program-locations' section."
 --TODO: [nice to have] better format control so we can pretty-print the
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
--
checkBadPerPackageCompilerPaths :: [ConfiguredProgram]
                                -> Map PackageName PackageConfig
                                -> IO ()
checkBadPerPackageCompilerPaths compilerPrograms packagesConfig =
    case [ (pkgname, progname)
         | let compProgNames = Set.fromList (map programId compilerPrograms)
         ,  (pkgname, pkgconf) <- Map.toList packagesConfig
         , progname <- Map.keys (getMapLast (packageConfigProgramPaths pkgconf))
         , progname `Set.member` compProgNames ] of
      [] -> return ()
      ps -> throwIO (BadPerPackageCompilerPaths ps)
