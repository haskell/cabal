{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.ProjectBuilding.PackageFileMonitor where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ProjectBuilding.Types
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad

import Distribution.Client.DistDirLayout
import Distribution.Client.FileMonitor
import Distribution.Client.Types hiding
  ( BuildFailure (..)
  , BuildOutcome
  , BuildOutcomes
  , BuildResult (..)
  )

import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Simple.InstallDirs (PathTemplate)
import Distribution.Simple.LocalBuildInfo
  ( ComponentName (..)
  )
import Distribution.Simple.Setup (HaddockTarget (ForDevelopment))
import Distribution.Simple.Utils (removeFileForcibly)

import qualified Data.Set as Set

-----------------------------
-- Package change detection
--

-- | Documentation output settings that, when changed, require Haddock to be
-- re-run. These are tracked separately from the package configuration key so
-- that changing them does not trigger a full package reconfiguration.
data HaddockDocSettings = HaddockDocSettings
  { hdsBuildHaddocks :: Bool
  , hdsHoogle :: Bool
  , hdsHtml :: Bool
  , hdsHtmlLocation :: Maybe String
  , hdsForeignLibs :: Bool
  , hdsForHackage :: HaddockTarget
  , hdsExecutables :: Bool
  , hdsTestSuites :: Bool
  , hdsBenchmarks :: Bool
  , hdsInternal :: Bool
  , hdsCss :: Maybe FilePath
  , hdsLinkedSource :: Bool
  , hdsQuickJump :: Bool
  , hdsHscolourCss :: Maybe FilePath
  , hdsContents :: Maybe PathTemplate
  , hdsIndex :: Maybe PathTemplate
  , hdsBaseUrl :: Maybe String
  , hdsResourcesDir :: Maybe String
  , hdsOutputDir :: Maybe FilePath
  , hdsUseUnicode :: Bool
  }
  deriving (Eq, Generic)

instance Binary HaddockDocSettings
instance Structured HaddockDocSettings

-- | The documentation settings of an elaborated package, as compared by
-- 'pkgFileMonitorDocs' to decide whether Haddock has to run again.
--
-- Settings that produce the same output compare equal: Haddock generates HTML
-- when no output format is selected, so that counts the same as an explicit
-- @--html@, which @haddock-project@ passes.
haddockDocSettings :: ElaboratedConfiguredPackage -> HaddockDocSettings
haddockDocSettings elab =
  HaddockDocSettings
    { hdsBuildHaddocks = elabBuildHaddocks elab
    , hdsHoogle = elabHaddockHoogle elab
    , -- Haddock defaults to HTML when neither output format is selected.
      -- haddock-project requests HTML explicitly; these are the same output.
      hdsHtml = elabHaddockHtml elab || not (elabHaddockHoogle elab)
    , hdsHtmlLocation = elabHaddockHtmlLocation elab
    , hdsForeignLibs = elabHaddockForeignLibs elab
    , hdsForHackage = elabHaddockForHackage elab
    , hdsExecutables = elabHaddockExecutables elab
    , hdsTestSuites = elabHaddockTestSuites elab
    , hdsBenchmarks = elabHaddockBenchmarks elab
    , hdsInternal = elabHaddockInternal elab
    , hdsCss = elabHaddockCss elab
    , hdsLinkedSource = elabHaddockLinkedSource elab
    , hdsQuickJump = elabHaddockQuickJump elab
    , hdsHscolourCss = elabHaddockHscolourCss elab
    , hdsContents = elabHaddockContents elab
    , hdsIndex = elabHaddockIndex elab
    , hdsBaseUrl = elabHaddockBaseUrl elab
    , hdsResourcesDir = elabHaddockResourcesDir elab
    , hdsOutputDir = elabHaddockOutputDir elab
    , hdsUseUnicode = elabHaddockUseUnicode elab
    }

-- | As part of the dry run for local unpacked packages we have to check if the
-- package config or files have changed. That is the purpose of
-- 'PackageFileMonitor' and 'checkPackageFileMonitorChanged'.
--
-- When a package is (re)built, the monitor must be updated to reflect the new
-- state of the package. Because we sometimes build without reconfiguring the
-- state updates are split into two, one for package config changes and one
-- for other changes. This is the purpose of 'updatePackageConfigFileMonitor'
-- and 'updatePackageBuildFileMonitor'.
data PackageFileMonitor = PackageFileMonitor
  { pkgFileMonitorConfig :: FileMonitor ElaboratedConfiguredPackage ()
  , pkgFileMonitorBuild :: FileMonitor (Set ComponentName, Set String) BuildResultMisc
  , pkgFileMonitorReg :: FileMonitor () (Maybe InstalledPackageInfo)
  , pkgFileMonitorDocs :: FileMonitor HaddockDocSettings ()
  }

-- | This is all the components of the 'BuildResult' other than the
-- @['InstalledPackageInfo']@.
--
-- We have to split up the 'BuildResult' components since they get produced
-- at different times (or rather, when different things change).
type BuildResultMisc = (DocsResult, TestsResult)

newPackageFileMonitor
  :: ElaboratedSharedConfig
  -> DistDirLayout
  -> DistDirParams
  -> PackageFileMonitor
newPackageFileMonitor
  shared
  DistDirLayout{distPackageCacheFile}
  dparams =
    PackageFileMonitor
      { pkgFileMonitorConfig =
          FileMonitor
            { fileMonitorCacheFile = distPackageCacheFile dparams "config"
            , fileMonitorKeyValid = (==) `on` normaliseConfiguredPackage shared
            , fileMonitorCheckIfOnlyValueChanged = False
            }
      , pkgFileMonitorBuild =
          FileMonitor
            { fileMonitorCacheFile = distPackageCacheFile dparams "build"
            , fileMonitorKeyValid = \(componentsToBuild, artifactsRequired) (componentsAlreadyBuilt, artifactsBuilt) ->
                componentsToBuild `Set.isSubsetOf` componentsAlreadyBuilt
                  && artifactsRequired `Set.isSubsetOf` artifactsBuilt
            , fileMonitorCheckIfOnlyValueChanged = True
            }
      , pkgFileMonitorReg =
          newFileMonitor (distPackageCacheFile dparams "registration")
      , pkgFileMonitorDocs =
          FileMonitor
            { fileMonitorCacheFile = distPackageCacheFile dparams "docs-config"
            , fileMonitorKeyValid = (==)
            , fileMonitorCheckIfOnlyValueChanged = False
            }
      }

-- | Helper function for 'checkPackageFileMonitorChanged',
-- 'updatePackageConfigFileMonitor' and 'updatePackageBuildFileMonitor'.
--
-- It selects the info from a 'ElaboratedConfiguredPackage' that are used by
-- the 'FileMonitor's (in the 'PackageFileMonitor') to detect value changes.
packageFileMonitorKeyValues
  :: ElaboratedConfiguredPackage
  -> (ElaboratedConfiguredPackage, Set ComponentName)
packageFileMonitorKeyValues elab =
  (elab_config, buildComponents)
  where
    -- The first part is the value used to guard (re)configuring the package.
    -- That is, if this value changes then we will reconfigure.
    -- The ElaboratedConfiguredPackage consists mostly (but not entirely) of
    -- information that affects the (re)configure step. But those parts that
    -- do not affect the configure step need to be nulled out. Those parts are
    -- the specific targets that we're going to build.
    --

    -- Also clear options passed when running Haddock, tests, or benchmarks.
    -- In particular, Haddock output options must not force reconfiguration
    -- when switching between a normal build and haddock-project. Compiler
    -- and program settings remain part of the key.
    --
    -- Automatic documentation arguments are supplied at invocation time and
    -- tracked by the build monitor. User arguments (including explicit
    -- -haddock and -fwrite-ide-info) must remain in the configuration key.

    -- TODO there may be more things to null here too, in the future.

    elab_config :: ElaboratedConfiguredPackage
    elab_config =
      elab
        { elabBuildTargets = []
        , elabTestTargets = []
        , elabBenchTargets = []
        , elabReplTarget = []
        , elabHaddockTargets = []
        , elabBuildHaddocks = False
        , elabHaddockProgramArgs = []
        , elabHaddockHoogle = False
        , elabHaddockHtml = False
        , elabHaddockHtmlLocation = Nothing
        , elabHaddockForeignLibs = False
        , elabHaddockForHackage = ForDevelopment
        , elabHaddockExecutables = False
        , elabHaddockTestSuites = False
        , elabHaddockBenchmarks = False
        , elabHaddockInternal = False
        , elabHaddockCss = Nothing
        , elabHaddockLinkedSource = False
        , elabHaddockQuickJump = False
        , elabHaddockHscolourCss = Nothing
        , elabHaddockContents = Nothing
        , elabHaddockIndex = Nothing
        , elabHaddockBaseUrl = Nothing
        , elabHaddockResourcesDir = Nothing
        , elabHaddockOutputDir = Nothing
        , elabHaddockUseUnicode = False
        , elabTestMachineLog = Nothing
        , elabTestHumanLog = Nothing
        , elabTestShowDetails = Nothing
        , elabTestKeepTix = False
        , elabTestTestOptions = []
        , elabBenchmarkOptions = []
        }

    -- The second part is the value used to guard the build step. So this is
    -- more or less the opposite of the first part, as it's just the info about
    -- what targets we're going to build.
    --
    buildComponents :: Set ComponentName
    buildComponents = elabBuildTargetWholeComponents elab

-- | Do all the checks on whether a package has changed and thus needs either
-- rebuilding or reconfiguring and rebuilding.
checkPackageFileMonitorChanged
  :: PackageFileMonitor
  -> ElaboratedConfiguredPackage
  -> FilePath
  -> [BuildStatus]
  -> IO (Either BuildStatusRebuild BuildResult)
checkPackageFileMonitorChanged
  PackageFileMonitor{..}
  pkg
  srcdir
  depsBuildStatus = do
    -- TODO: [nice to have] some debug-level message about file
    -- changes, like rerunIfChanged
    configChanged <-
      checkFileMonitorChanged
        pkgFileMonitorConfig
        srcdir
        pkgconfig
    case configChanged of
      MonitorChanged monitorReason ->
        return (Left (BuildStatusConfigure $ void monitorReason))
      MonitorUnchanged () _
        -- The configChanged here includes the identity of the dependencies,
        -- so depsBuildStatus is just needed for the changes in the content
        -- of dependencies.
        | any buildStatusRequiresBuild depsBuildStatus -> do
            regChanged <- checkFileMonitorChanged pkgFileMonitorReg srcdir ()
            let mreg = changedToMaybe regChanged
            return (Left (BuildStatusBuild mreg BuildReasonDepsRebuilt))
        | otherwise -> do
            buildChanged <-
              checkFileMonitorChanged
                pkgFileMonitorBuild
                srcdir
                (buildComponents, buildArtifacts)
            regChanged <-
              checkFileMonitorChanged
                pkgFileMonitorReg
                srcdir
                ()
            let mreg = changedToMaybe regChanged
            case (buildChanged, regChanged) of
              (MonitorChanged (MonitoredValueChanged (prevBuildComponents, prevBuildArtifacts)), _) ->
                return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason
                    | buildArtifacts `Set.isSubsetOf` prevBuildArtifacts = BuildReasonExtraTargets prevBuildComponents
                    | otherwise = BuildReasonDocumentation
              (MonitorChanged monitorReason, _) ->
                return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason = BuildReasonFilesChanged $ void monitorReason
              (MonitorUnchanged _ _, MonitorChanged monitorReason) ->
                -- this should only happen if the file is corrupt or been
                -- manually deleted. We don't want to bother with another
                -- phase just for this, so we'll reregister by doing a build.
                return (Left (BuildStatusBuild Nothing buildReason))
                where
                  buildReason = BuildReasonFilesChanged $ void monitorReason
              (MonitorUnchanged _ _, MonitorUnchanged _ _)
                | pkgHasEphemeralBuildTargets pkg ->
                    return (Left (BuildStatusBuild mreg buildReason))
                where
                  buildReason = BuildReasonEphemeralTargets
              (MonitorUnchanged buildResult _, MonitorUnchanged _ _) -> do
                docsChanged <-
                  if hasValidHaddockTargets pkg
                    then checkFileMonitorChanged pkgFileMonitorDocs srcdir (haddockDocSettings pkg)
                    else return (MonitorUnchanged () [])
                case docsChanged of
                  MonitorChanged _ ->
                    return (Left (BuildStatusBuild mreg buildReason))
                    where
                      buildReason = BuildReasonHaddockSettingsChanged
                  MonitorUnchanged _ _ ->
                    return $
                      Right
                        BuildResult
                          { buildResultDocs = docsResult
                          , buildResultTests = testsResult
                          , buildResultLogFile = Nothing
                          , buildResultBenchmark = Nothing
                          }
                where
                  (docsResult, testsResult) = buildResult
    where
      (pkgconfig, buildComponents) = packageFileMonitorKeyValues pkg
      buildArtifacts = Set.fromList (elabHaddockProgramArgs pkg)
      changedToMaybe :: MonitorChanged a b -> Maybe b
      changedToMaybe (MonitorChanged _) = Nothing
      changedToMaybe (MonitorUnchanged x _) = Just x

updatePackageConfigFileMonitor
  :: PackageFileMonitor
  -> FilePath
  -> ElaboratedConfiguredPackage
  -> IO ()
updatePackageConfigFileMonitor
  PackageFileMonitor{pkgFileMonitorConfig}
  srcdir
  pkg =
    updateFileMonitor
      pkgFileMonitorConfig
      srcdir
      Nothing
      []
      pkgconfig
      ()
    where
      (pkgconfig, _buildComponents) = packageFileMonitorKeyValues pkg

updatePackageBuildFileMonitor
  :: PackageFileMonitor
  -> FilePath
  -> MonitorTimestamp
  -> ElaboratedConfiguredPackage
  -> BuildStatusRebuild
  -> [MonitorFilePath]
  -> BuildResultMisc
  -> IO ()
updatePackageBuildFileMonitor
  PackageFileMonitor{pkgFileMonitorBuild}
  srcdir
  timestamp
  pkg
  pkgBuildStatus
  monitors
  buildResult =
    -- Called only for the compilation phase, including compilation required
    -- by ephemeral commands. Never leave an old successful build cache valid
    -- after replacing its artifacts.
    updateFileMonitor
      pkgFileMonitorBuild
      srcdir
      (Just timestamp)
      monitors
      (buildComponents', Set.fromList (elabHaddockProgramArgs pkg))
      buildResult
    where
      (_pkgconfig, buildComponents) = packageFileMonitorKeyValues pkg

      -- If the only thing that's changed is that we're now building extra
      -- components, then we can avoid later unnecessary rebuilds by saving the
      -- total set of components that have been built, namely the union of the
      -- existing ones plus the new ones. If files also changed this would be
      -- the wrong thing to do. Note that we rely on the
      -- fileMonitorCheckIfOnlyValueChanged = True mode to get this guarantee
      -- that it's /only/ the value that changed not any files that changed.
      buildComponents' =
        case pkgBuildStatus of
          BuildStatusBuild _ (BuildReasonExtraTargets prevBuildComponents) ->
            buildComponents `Set.union` prevBuildComponents
          _ -> buildComponents

-- | Remember the persistent documentation configuration after Haddock succeeds.
-- Ephemeral commands can supply their own output settings (e.g. the index URLs
-- used by haddock-project); these must not replace the settings remembered for
-- normal builds. Explicit Haddock targets run unconditionally anyway.
updatePackageDocsFileMonitor :: PackageFileMonitor -> FilePath -> ElaboratedConfiguredPackage -> IO ()
updatePackageDocsFileMonitor PackageFileMonitor{pkgFileMonitorDocs} srcdir pkg =
  unless (pkgHasEphemeralBuildTargets pkg) $
    -- TODO: monitor the files that only Haddock reads, e.g. the
    -- extra-doc-files and those reported by the Haddock phase (which are
    -- discarded by 'withoutBuildMonitor'). Only the documentation settings
    -- are monitored, so changing such a file does not regenerate the
    -- documentation of a build with documentation enabled.
    updateFileMonitor pkgFileMonitorDocs srcdir Nothing [] (haddockDocSettings pkg) ()

-- | Invalidate 'pkgFileMonitorBuild' by removing its cache file, so that the
-- next 'checkPackageFileMonitorChanged' treats it as a first run and builds
-- the package again.
invalidatePackageBuildFileMonitor :: PackageFileMonitor -> IO ()
invalidatePackageBuildFileMonitor PackageFileMonitor{pkgFileMonitorBuild} =
  removeFileForcibly (fileMonitorCacheFile pkgFileMonitorBuild)

updatePackageRegFileMonitor
  :: PackageFileMonitor
  -> FilePath
  -> Maybe InstalledPackageInfo
  -> IO ()
updatePackageRegFileMonitor
  PackageFileMonitor{pkgFileMonitorReg}
  srcdir
  mipkg =
    updateFileMonitor
      pkgFileMonitorReg
      srcdir
      Nothing
      []
      ()
      mipkg

invalidatePackageRegFileMonitor :: PackageFileMonitor -> IO ()
invalidatePackageRegFileMonitor PackageFileMonitor{pkgFileMonitorReg} =
  removeFileForcibly (fileMonitorCacheFile pkgFileMonitorReg)
