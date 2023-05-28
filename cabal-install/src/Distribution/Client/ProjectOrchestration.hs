{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module deals with building and incrementally rebuilding a collection
-- of packages. It is what backs the @cabal build@ and @configure@ commands,
-- as well as being a core part of @run@, @test@, @bench@ and others.
--
-- The primary thing is in fact rebuilding (and trying to make that quick by
-- not redoing unnecessary work), so building from scratch is just a special
-- case.
--
-- The build process and the code can be understood by breaking it down into
-- three major parts:
--
-- * The 'ElaboratedInstallPlan' type
--
-- * The \"what to do\" phase, where we look at the all input configuration
--   (project files, .cabal files, command line etc) and produce a detailed
--   plan of what to do -- the 'ElaboratedInstallPlan'.
--
-- * The \"do it\" phase, where we take the 'ElaboratedInstallPlan' and we
-- re-execute it.
--
-- As far as possible, the \"what to do\" phase embodies all the policy, leaving
-- the \"do it\" phase policy free. The first phase contains more of the
-- complicated logic, but it is contained in code that is either pure or just
-- has read effects (except cache updates). Then the second phase does all the
-- actions to build packages, but as far as possible it just follows the
-- instructions and avoids any logic for deciding what to do (apart from
-- recompilation avoidance in executing the plan).
--
-- This division helps us keep the code under control, making it easier to
-- understand, test and debug. So when you are extending these modules, please
-- think about which parts of your change belong in which part. It is
-- perfectly ok to extend the description of what to do (i.e. the
-- 'ElaboratedInstallPlan') if that helps keep the policy decisions in the
-- first phase. Also, the second phase does not have direct access to any of
-- the input configuration anyway; all the information has to flow via the
-- 'ElaboratedInstallPlan'.
module Distribution.Client.ProjectOrchestration
  ( -- * Discovery phase: what is in the project?
    CurrentCommand (..)
  , establishProjectBaseContext
  , establishProjectBaseContextWithRoot
  , ProjectBaseContext (..)
  , BuildTimeSettings (..)
  , commandLineFlagsToProjectConfig

    -- * Pre-build phase: decide what to do.
  , withInstallPlan
  , runProjectPreBuildPhase
  , ProjectBuildContext (..)

    -- ** Selecting what targets we mean
  , readTargetSelectors
  , reportTargetSelectorProblems
  , resolveTargets
  , TargetsMap
  , allTargetSelectors
  , uniqueTargetSelectors
  , TargetSelector (..)
  , TargetImplicitCwd (..)
  , PackageId
  , AvailableTarget (..)
  , AvailableTargetStatus (..)
  , TargetRequested (..)
  , ComponentName (..)
  , ComponentKind (..)
  , ComponentTarget (..)
  , SubComponentTarget (..)
  , selectComponentTargetBasic
  , distinctTargetComponents

    -- ** Utils for selecting targets
  , filterTargetsKind
  , filterTargetsKindWith
  , selectBuildableTargets
  , selectBuildableTargetsWith
  , selectBuildableTargets'
  , selectBuildableTargetsWith'
  , forgetTargetsDetail

    -- ** Adjusting the plan
  , pruneInstallPlanToTargets
  , TargetAction (..)
  , pruneInstallPlanToDependencies
  , CannotPruneDependencies (..)
  , printPlan

    -- * Build phase: now do it.
  , runProjectBuildPhase

    -- * Post build actions
  , runProjectPostBuildPhase
  , dieOnBuildFailures

    -- * Dummy projects
  , establishDummyProjectBaseContext
  , establishDummyDistDirLayout
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Directory
  ( makeAbsolute
  )
import Prelude ()

import Distribution.Client.ProjectBuilding
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanOutput
import Distribution.Client.ProjectPlanning hiding
  ( pruneInstallPlanToTargets
  )
import qualified Distribution.Client.ProjectPlanning as ProjectPlanning
  ( pruneInstallPlanToTargets
  )
import Distribution.Client.ProjectPlanning.Types

import Distribution.Client.DistDirLayout
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  )
import Distribution.Client.TargetSelector
  ( ComponentKind (..)
  , TargetImplicitCwd (..)
  , TargetSelector (..)
  , componentKind
  , readTargetSelectors
  , reportTargetSelectorProblems
  )
import Distribution.Client.Types
  ( DocsResult (..)
  , GenericReadyPackage (..)
  , PackageLocation (..)
  , PackageSpecifier (..)
  , SourcePackageDb (..)
  , TestsResult (..)
  , UnresolvedSourcePackage
  , WriteGhcEnvironmentFilesPolicy (..)
  )
import Distribution.Solver.Types.PackageIndex
  ( lookupPackageName
  )

import Distribution.Client.BuildReports.Anonymous (cabalInstallID)
import qualified Distribution.Client.BuildReports.Anonymous as BuildReports
import qualified Distribution.Client.BuildReports.Storage as BuildReports
  ( storeLocal
  )

import Distribution.Client.HttpUtils
import Distribution.Client.Setup hiding (packageName)
import Distribution.Compiler
  ( CompilerFlavor (GHC)
  )
import Distribution.Types.ComponentName
  ( componentNameString
  )
import Distribution.Types.InstalledPackageInfo
  ( InstalledPackageInfo
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  , packageNameToUnqualComponentName
  )

import Distribution.Solver.Types.OptionalStanza

import Distribution.Package
import Distribution.Simple.Command (commandShowOptions)
import Distribution.Simple.Compiler
  ( OptimisationLevel (..)
  , compilerCompatVersion
  , compilerId
  , compilerInfo
  , showCompilerId
  )
import Distribution.Simple.Configure (computeEffectiveProfiling)
import Distribution.Simple.Flag
  ( flagToMaybe
  , fromFlagOrDefault
  )
import Distribution.Simple.LocalBuildInfo
  ( ComponentName (..)
  , pkgComponents
  )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , debugNoWrap
  , die'
  , notice
  , noticeNoWrap
  , ordNub
  , warn
  )
import Distribution.System
  ( Platform (Platform)
  )
import Distribution.Types.Flag
  ( FlagAssignment
  , diffFlagAssignment
  , showFlagAssignment
  )
import Distribution.Utils.NubList
  ( fromNubList
  )
import Distribution.Verbosity
import Distribution.Version
  ( mkVersion
  )

import Control.Exception (assert)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
#ifdef MIN_VERSION_unix
import           System.Posix.Signals (sigKILL, sigSEGV)
#endif

-- | Tracks what command is being executed, because we need to hide this somewhere
-- for cases that need special handling (usually for error reporting).
data CurrentCommand = InstallCommand | HaddockCommand | BuildCommand | ReplCommand | OtherCommand
  deriving (Show, Eq)

-- | This holds the context of a project prior to solving: the content of the
-- @cabal.project@ and all the local package @.cabal@ files.
data ProjectBaseContext = ProjectBaseContext
  { distDirLayout :: DistDirLayout
  , cabalDirLayout :: CabalDirLayout
  , projectConfig :: ProjectConfig
  , localPackages :: [PackageSpecifier UnresolvedSourcePackage]
  , buildSettings :: BuildTimeSettings
  , currentCommand :: CurrentCommand
  , installedPackages :: Maybe InstalledPackageIndex
  }

establishProjectBaseContext
  :: Verbosity
  -> ProjectConfig
  -> CurrentCommand
  -> IO ProjectBaseContext
establishProjectBaseContext verbosity cliConfig currentCommand = do
  projectRoot <- either throwIO return =<< findProjectRoot verbosity mprojectDir mprojectFile
  establishProjectBaseContextWithRoot verbosity cliConfig projectRoot currentCommand
  where
    mprojectDir = Setup.flagToMaybe projectConfigProjectDir
    mprojectFile = Setup.flagToMaybe projectConfigProjectFile
    ProjectConfigShared{projectConfigProjectDir, projectConfigProjectFile} = projectConfigShared cliConfig

-- | Like 'establishProjectBaseContext' but doesn't search for project root.
establishProjectBaseContextWithRoot
  :: Verbosity
  -> ProjectConfig
  -> ProjectRoot
  -> CurrentCommand
  -> IO ProjectBaseContext
establishProjectBaseContextWithRoot verbosity cliConfig projectRoot currentCommand = do
  let haddockOutputDir = flagToMaybe (packageConfigHaddockOutputDir (projectConfigLocalPackages cliConfig))
  let distDirLayout = defaultDistDirLayout projectRoot mdistDirectory haddockOutputDir

  httpTransport <-
    configureTransport
      verbosity
      (fromNubList . projectConfigProgPathExtra $ projectConfigShared cliConfig)
      (flagToMaybe . projectConfigHttpTransport $ projectConfigBuildOnly cliConfig)

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      verbosity
      httpTransport
      distDirLayout
      cliConfig

  let ProjectConfigBuildOnly
        { projectConfigLogsDir
        } = projectConfigBuildOnly projectConfig

      ProjectConfigShared
        { projectConfigStoreDir
        } = projectConfigShared projectConfig

      mlogsDir = Setup.flagToMaybe projectConfigLogsDir
  mstoreDir <-
    sequenceA $
      makeAbsolute
        <$> Setup.flagToMaybe projectConfigStoreDir
  cabalDirLayout <- mkCabalDirLayout mstoreDir mlogsDir

  let buildSettings =
        resolveBuildTimeSettings
          verbosity
          cabalDirLayout
          projectConfig

  -- https://github.com/haskell/cabal/issues/6013
  when (null (projectPackages projectConfig) && null (projectPackagesOptional projectConfig)) $
    warn verbosity "There are no packages or optional-packages in the project"

  return
    ProjectBaseContext
      { distDirLayout
      , cabalDirLayout
      , projectConfig
      , localPackages
      , buildSettings
      , currentCommand
      , installedPackages
      }
  where
    mdistDirectory = Setup.flagToMaybe projectConfigDistDir
    ProjectConfigShared{projectConfigDistDir} = projectConfigShared cliConfig
    installedPackages = Nothing

-- | This holds the context between the pre-build, build and post-build phases.
data ProjectBuildContext = ProjectBuildContext
  { elaboratedPlanOriginal :: ElaboratedInstallPlan
  -- ^ This is the improved plan, before we select a plan subset based on
  -- the build targets, and before we do the dry-run. So this contains
  -- all packages in the project.
  , elaboratedPlanToExecute :: ElaboratedInstallPlan
  -- ^ This is the 'elaboratedPlanOriginal' after we select a plan subset
  -- and do the dry-run phase to find out what is up-to or out-of date.
  -- This is the plan that will be executed during the build phase. So
  -- this contains only a subset of packages in the project.
  , elaboratedShared :: ElaboratedSharedConfig
  -- ^ The part of the install plan that's shared between all packages in
  -- the plan. This does not change between the two plan variants above,
  -- so there is just the one copy.
  , pkgsBuildStatus :: BuildStatusMap
  -- ^ The result of the dry-run phase. This tells us about each member of
  -- the 'elaboratedPlanToExecute'.
  , targetsMap :: TargetsMap
  -- ^ The targets selected by @selectPlanSubset@. This is useful eg. in
  -- CmdRun, where we need a valid target to execute.
  }

-- | Pre-build phase: decide what to do.
withInstallPlan
  :: Verbosity
  -> ProjectBaseContext
  -> (ElaboratedInstallPlan -> ElaboratedSharedConfig -> IO a)
  -> IO a
withInstallPlan
  verbosity
  ProjectBaseContext
    { distDirLayout
    , cabalDirLayout
    , projectConfig
    , localPackages
    , installedPackages
    }
  action = do
    -- Take the project configuration and make a plan for how to build
    -- everything in the project. This is independent of any specific targets
    -- the user has asked for.
    --
    (elaboratedPlan, _, elaboratedShared, _, _) <-
      rebuildInstallPlan
        verbosity
        distDirLayout
        cabalDirLayout
        projectConfig
        localPackages
        installedPackages
    action elaboratedPlan elaboratedShared

runProjectPreBuildPhase
  :: Verbosity
  -> ProjectBaseContext
  -> (ElaboratedInstallPlan -> IO (ElaboratedInstallPlan, TargetsMap))
  -> IO ProjectBuildContext
runProjectPreBuildPhase
  verbosity
  ProjectBaseContext
    { distDirLayout
    , cabalDirLayout
    , projectConfig
    , localPackages
    , installedPackages
    }
  selectPlanSubset = do
    -- Take the project configuration and make a plan for how to build
    -- everything in the project. This is independent of any specific targets
    -- the user has asked for.
    --
    (elaboratedPlan, _, elaboratedShared, _, _) <-
      rebuildInstallPlan
        verbosity
        distDirLayout
        cabalDirLayout
        projectConfig
        localPackages
        installedPackages

    -- The plan for what to do is represented by an 'ElaboratedInstallPlan'

    -- Now given the specific targets the user has asked for, decide
    -- which bits of the plan we will want to execute.
    --
    (elaboratedPlan', targets) <- selectPlanSubset elaboratedPlan

    -- Check which packages need rebuilding.
    -- This also gives us more accurate reasons for the --dry-run output.
    --
    pkgsBuildStatus <-
      rebuildTargetsDryRun
        distDirLayout
        elaboratedShared
        elaboratedPlan'

    -- Improve the plan by marking up-to-date packages as installed.
    --
    let elaboratedPlan'' =
          improveInstallPlanWithUpToDatePackages
            pkgsBuildStatus
            elaboratedPlan'
    debugNoWrap verbosity (InstallPlan.showInstallPlan elaboratedPlan'')

    return
      ProjectBuildContext
        { elaboratedPlanOriginal = elaboratedPlan
        , elaboratedPlanToExecute = elaboratedPlan''
        , elaboratedShared
        , pkgsBuildStatus
        , targetsMap = targets
        }

-- | Build phase: now do it.
--
-- Execute all or parts of the description of what to do to build or
-- rebuild the various packages needed.
runProjectBuildPhase
  :: Verbosity
  -> ProjectBaseContext
  -> ProjectBuildContext
  -> IO BuildOutcomes
runProjectBuildPhase _ ProjectBaseContext{buildSettings} _
  | buildSettingDryRun buildSettings =
      return Map.empty
runProjectBuildPhase
  verbosity
  ProjectBaseContext{..}
  ProjectBuildContext{..} =
    fmap (Map.union (previousBuildOutcomes pkgsBuildStatus)) $
      rebuildTargets
        verbosity
        projectConfig
        distDirLayout
        (cabalStoreDirLayout cabalDirLayout)
        elaboratedPlanToExecute
        elaboratedShared
        pkgsBuildStatus
        buildSettings
    where
      previousBuildOutcomes :: BuildStatusMap -> BuildOutcomes
      previousBuildOutcomes =
        Map.mapMaybe $ \status -> case status of
          BuildStatusUpToDate buildSuccess -> Just (Right buildSuccess)
          -- TODO: [nice to have] record build failures persistently
          _ -> Nothing

-- | Post-build phase: various administrative tasks
--
-- Update bits of state based on the build outcomes and report any failures.
runProjectPostBuildPhase
  :: Verbosity
  -> ProjectBaseContext
  -> ProjectBuildContext
  -> BuildOutcomes
  -> IO ()
runProjectPostBuildPhase _ ProjectBaseContext{buildSettings} _ _
  | buildSettingDryRun buildSettings =
      return ()
runProjectPostBuildPhase
  verbosity
  ProjectBaseContext{..}
  bc@ProjectBuildContext{..}
  buildOutcomes = do
    -- Update other build artefacts
    -- TODO: currently none, but could include:
    --        - bin symlinks/wrappers
    --        - haddock/hoogle/ctags indexes
    --        - delete stale lib registrations
    --        - delete stale package dirs

    postBuildStatus <-
      updatePostBuildProjectStatus
        verbosity
        distDirLayout
        elaboratedPlanOriginal
        pkgsBuildStatus
        buildOutcomes

    -- Write the .ghc.environment file (if allowed by the env file write policy).
    let writeGhcEnvFilesPolicy =
          projectConfigWriteGhcEnvironmentFilesPolicy . projectConfigShared $
            projectConfig

        shouldWriteGhcEnvironment :: Bool
        shouldWriteGhcEnvironment =
          case fromFlagOrDefault
            NeverWriteGhcEnvironmentFiles
            writeGhcEnvFilesPolicy of
            AlwaysWriteGhcEnvironmentFiles -> True
            NeverWriteGhcEnvironmentFiles -> False
            WriteGhcEnvironmentFilesOnlyForGhc844AndNewer ->
              let compiler = pkgConfigCompiler elaboratedShared
                  ghcCompatVersion = compilerCompatVersion GHC compiler
               in maybe False (>= mkVersion [8, 4, 4]) ghcCompatVersion

    when shouldWriteGhcEnvironment $
      void $
        writePlanGhcEnvironment
          (distProjectRootDirectory distDirLayout)
          elaboratedPlanOriginal
          elaboratedShared
          postBuildStatus

    -- Write the build reports
    writeBuildReports buildSettings bc elaboratedPlanToExecute buildOutcomes

    -- Finally if there were any build failures then report them and throw
    -- an exception to terminate the program
    dieOnBuildFailures verbosity currentCommand elaboratedPlanToExecute buildOutcomes

-- Note that it is a deliberate design choice that the 'buildTargets' is
-- not passed to phase 1, and the various bits of input config is not
-- passed to phase 2.
--
-- We make the install plan without looking at the particular targets the
-- user asks us to build. The set of available things we can build is
-- discovered from the env and config and is used to make the install plan.
-- The targets just tell us which parts of the install plan to execute.
--
-- Conversely, executing the plan does not directly depend on any of the
-- input config. The bits that are needed (or better, the decisions based
-- on it) all go into the install plan.

-- Notionally, the 'BuildFlags' should be things that do not affect what
-- we build, just how we do it. These ones of course do

------------------------------------------------------------------------------
-- Taking targets into account, selecting what to build
--

-- | The set of components to build, represented as a mapping from 'UnitId's
-- to the 'ComponentTarget's within the unit that will be selected
-- (e.g. selected to build, test or repl).
--
-- Associated with each 'ComponentTarget' is the set of 'TargetSelector's that
-- matched this target. Typically this is exactly one, but in general it is
-- possible to for different selectors to match the same target. This extra
-- information is primarily to help make helpful error messages.
type TargetsMap = Map UnitId [(ComponentTarget, NonEmpty TargetSelector)]

-- | Get all target selectors.
allTargetSelectors :: TargetsMap -> [TargetSelector]
allTargetSelectors = concatMap (NE.toList . snd) . concat . Map.elems

-- | Get all unique target selectors.
uniqueTargetSelectors :: TargetsMap -> [TargetSelector]
uniqueTargetSelectors = ordNub . allTargetSelectors

-- | Given a set of 'TargetSelector's, resolve which 'UnitId's and
-- 'ComponentTarget's they ought to refer to.
--
-- The idea is that every user target identifies one or more roots in the
-- 'ElaboratedInstallPlan', which we will use to determine the closure
-- of what packages need to be built, dropping everything from the plan
-- that is unnecessary. This closure and pruning is done by
-- 'pruneInstallPlanToTargets' and this needs to be told the roots in terms
-- of 'UnitId's and the 'ComponentTarget's within those.
--
-- This means we first need to translate the 'TargetSelector's into the
-- 'UnitId's and 'ComponentTarget's. This translation has to be different for
-- the different command line commands, like @build@, @repl@ etc. For example
-- the command @build pkgfoo@ could select a different set of components in
-- pkgfoo than @repl pkgfoo@. The @build@ command would select any library and
-- all executables, whereas @repl@ would select the library or a single
-- executable. Furthermore, both of these examples could fail, and fail in
-- different ways and each needs to be able to produce helpful error messages.
--
-- So 'resolveTargets' takes two helpers: one to select the targets to be used
-- by user targets that refer to a whole package ('TargetPackage'), and
-- another to check user targets that refer to a component (or a module or
-- file within a component). These helpers can fail, and use their own error
-- type. Both helpers get given the 'AvailableTarget' info about the
-- component(s).
--
-- While commands vary quite a bit in their behaviour about which components to
-- select for a whole-package target, most commands have the same behaviour for
-- checking a user target that refers to a specific component. To help with
-- this commands can use 'selectComponentTargetBasic', either directly or as
-- a basis for their own @selectComponentTarget@ implementation.
resolveTargets
  :: forall err
   . ( forall k
        . TargetSelector
       -> [AvailableTarget k]
       -> Either (TargetProblem err) [k]
     )
  -> ( forall k
        . SubComponentTarget
       -> AvailableTarget k
       -> Either (TargetProblem err) k
     )
  -> ElaboratedInstallPlan
  -> Maybe (SourcePackageDb)
  -> [TargetSelector]
  -> Either [TargetProblem err] TargetsMap
resolveTargets
  selectPackageTargets
  selectComponentTarget
  installPlan
  mPkgDb =
    fmap mkTargetsMap
      . either (Left . toList) Right
      . checkErrors
      . map (\ts -> (,) ts <$> checkTarget ts)
    where
      mkTargetsMap
        :: [(TargetSelector, [(UnitId, ComponentTarget)])]
        -> TargetsMap
      mkTargetsMap targets =
        Map.map nubComponentTargets $
          Map.fromListWith
            (<>)
            [ (uid, [(ct, ts)])
            | (ts, cts) <- targets
            , (uid, ct) <- cts
            ]

      AvailableTargetIndexes{..} = availableTargetIndexes installPlan

      checkTarget :: TargetSelector -> Either (TargetProblem err) [(UnitId, ComponentTarget)]

      -- We can ask to build any whole package, project-local or a dependency
      checkTarget bt@(TargetPackage _ [pkgid] mkfilter)
        | Just ats <-
            fmap (maybe id filterTargetsKind mkfilter) $
              Map.lookup pkgid availableTargetsByPackageId =
            fmap (componentTargets WholeComponent) $
              selectPackageTargets bt ats
        | otherwise =
            Left (TargetProblemNoSuchPackage pkgid)
      checkTarget (TargetPackage _ pkgids _) =
        error
          ( "TODO: add support for multiple packages in a directory.  Got\n"
              ++ unlines (map prettyShow pkgids)
          )
      -- For the moment this error cannot happen here, because it gets
      -- detected when the package config is being constructed. This case
      -- will need handling properly when we do add support.
      --
      -- TODO: how should this use case play together with the
      -- '--cabal-file' option of 'configure' which allows using multiple
      -- .cabal files for a single package?

      checkTarget bt@(TargetAllPackages mkfilter) =
        fmap (componentTargets WholeComponent)
          . selectPackageTargets bt
          . maybe id filterTargetsKind mkfilter
          . filter availableTargetLocalToProject
          $ concat (Map.elems availableTargetsByPackageId)
      checkTarget (TargetComponent pkgid cname subtarget)
        | Just ats <-
            Map.lookup
              (pkgid, cname)
              availableTargetsByPackageIdAndComponentName =
            fmap (componentTargets subtarget) $
              selectComponentTargets subtarget ats
        | Map.member pkgid availableTargetsByPackageId =
            Left (TargetProblemNoSuchComponent pkgid cname)
        | otherwise =
            Left (TargetProblemNoSuchPackage pkgid)
      checkTarget (TargetComponentUnknown pkgname ecname subtarget)
        | Just ats <- case ecname of
            Left ucname ->
              Map.lookup
                (pkgname, ucname)
                availableTargetsByPackageNameAndUnqualComponentName
            Right cname ->
              Map.lookup
                (pkgname, cname)
                availableTargetsByPackageNameAndComponentName =
            fmap (componentTargets subtarget) $
              selectComponentTargets subtarget ats
        | Map.member pkgname availableTargetsByPackageName =
            Left (TargetProblemUnknownComponent pkgname ecname)
        | otherwise =
            Left (TargetNotInProject pkgname)
      checkTarget bt@(TargetPackageNamed pkgname mkfilter)
        | Just ats <-
            fmap (maybe id filterTargetsKind mkfilter) $
              Map.lookup pkgname availableTargetsByPackageName =
            fmap (componentTargets WholeComponent)
              . selectPackageTargets bt
              $ ats
        | Just SourcePackageDb{packageIndex} <- mPkgDb
        , let pkg = lookupPackageName packageIndex pkgname
        , not (null pkg) =
            Left (TargetAvailableInIndex pkgname)
        | otherwise =
            Left (TargetNotInProject pkgname)

      componentTargets
        :: SubComponentTarget
        -> [(b, ComponentName)]
        -> [(b, ComponentTarget)]
      componentTargets subtarget =
        map (fmap (\cname -> ComponentTarget cname subtarget))

      selectComponentTargets
        :: SubComponentTarget
        -> [AvailableTarget k]
        -> Either (TargetProblem err) [k]
      selectComponentTargets subtarget =
        either (Left . NE.head) Right
          . checkErrors
          . map (selectComponentTarget subtarget)

      checkErrors :: [Either e a] -> Either (NonEmpty e) [a]
      checkErrors =
        (\(es, xs) -> case es of [] -> Right xs; (e : es') -> Left (e :| es'))
          . partitionEithers

data AvailableTargetIndexes = AvailableTargetIndexes
  { availableTargetsByPackageIdAndComponentName
      :: AvailableTargetsMap (PackageId, ComponentName)
  , availableTargetsByPackageId
      :: AvailableTargetsMap PackageId
  , availableTargetsByPackageName
      :: AvailableTargetsMap PackageName
  , availableTargetsByPackageNameAndComponentName
      :: AvailableTargetsMap (PackageName, ComponentName)
  , availableTargetsByPackageNameAndUnqualComponentName
      :: AvailableTargetsMap (PackageName, UnqualComponentName)
  }
type AvailableTargetsMap k = Map k [AvailableTarget (UnitId, ComponentName)]

-- We define a bunch of indexes to help 'resolveTargets' with resolving
-- 'TargetSelector's to specific 'UnitId's.
--
-- They are all derived from the 'availableTargets' index.
-- The 'availableTargetsByPackageIdAndComponentName' is just that main index,
-- while the others are derived by re-grouping on the index key.
--
-- They are all constructed lazily because they are not necessarily all used.
--
availableTargetIndexes :: ElaboratedInstallPlan -> AvailableTargetIndexes
availableTargetIndexes installPlan = AvailableTargetIndexes{..}
  where
    availableTargetsByPackageIdAndComponentName
      :: Map
          (PackageId, ComponentName)
          [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByPackageIdAndComponentName =
      availableTargets installPlan

    availableTargetsByPackageId
      :: Map PackageId [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByPackageId =
      Map.mapKeysWith
        (++)
        (\(pkgid, _cname) -> pkgid)
        availableTargetsByPackageIdAndComponentName
        `Map.union` availableTargetsEmptyPackages

    availableTargetsByPackageName
      :: Map PackageName [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByPackageName =
      Map.mapKeysWith
        (++)
        packageName
        availableTargetsByPackageId

    availableTargetsByPackageNameAndComponentName
      :: Map
          (PackageName, ComponentName)
          [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByPackageNameAndComponentName =
      Map.mapKeysWith
        (++)
        (\(pkgid, cname) -> (packageName pkgid, cname))
        availableTargetsByPackageIdAndComponentName

    availableTargetsByPackageNameAndUnqualComponentName
      :: Map
          (PackageName, UnqualComponentName)
          [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByPackageNameAndUnqualComponentName =
      Map.mapKeysWith
        (++)
        ( \(pkgid, cname) ->
            let pname = packageName pkgid
                cname' = unqualComponentName pname cname
             in (pname, cname')
        )
        availableTargetsByPackageIdAndComponentName
      where
        unqualComponentName
          :: PackageName -> ComponentName -> UnqualComponentName
        unqualComponentName pkgname =
          fromMaybe (packageNameToUnqualComponentName pkgname)
            . componentNameString

    -- Add in all the empty packages. These do not appear in the
    -- availableTargetsByComponent map, since that only contains
    -- components, so packages with no components are invisible from
    -- that perspective.  The empty packages need to be there for
    -- proper error reporting, so users can select the empty package
    -- and then we can report that it is empty, otherwise we falsely
    -- report there is no such package at all.
    availableTargetsEmptyPackages =
      Map.fromList
        [ (packageId pkg, [])
        | InstallPlan.Configured pkg <- InstallPlan.toList installPlan
        , case elabPkgOrComp pkg of
            ElabComponent _ -> False
            ElabPackage _ -> null (pkgComponents (elabPkgDescription pkg))
        ]

-- TODO: [research required] what if the solution has multiple
--      versions of this package?
--      e.g. due to setup deps or due to multiple independent sets
--      of packages being built (e.g. ghc + ghcjs in a project)

filterTargetsKind :: ComponentKind -> [AvailableTarget k] -> [AvailableTarget k]
filterTargetsKind ckind = filterTargetsKindWith (== ckind)

filterTargetsKindWith
  :: (ComponentKind -> Bool)
  -> [AvailableTarget k]
  -> [AvailableTarget k]
filterTargetsKindWith p ts =
  [ t | t@(AvailableTarget _ cname _ _) <- ts, p (componentKind cname)
  ]

selectBuildableTargets :: [AvailableTarget k] -> [k]
selectBuildableTargets = selectBuildableTargetsWith (const True)

zipBuildableTargetsWith
  :: (TargetRequested -> Bool)
  -> [AvailableTarget k]
  -> [(k, AvailableTarget k)]
zipBuildableTargetsWith p ts =
  [(k, t) | t@(AvailableTarget _ _ (TargetBuildable k req) _) <- ts, p req]

selectBuildableTargetsWith
  :: (TargetRequested -> Bool)
  -> [AvailableTarget k]
  -> [k]
selectBuildableTargetsWith p = map fst . zipBuildableTargetsWith p

selectBuildableTargets' :: [AvailableTarget k] -> ([k], [AvailableTarget ()])
selectBuildableTargets' = selectBuildableTargetsWith' (const True)

selectBuildableTargetsWith'
  :: (TargetRequested -> Bool)
  -> [AvailableTarget k]
  -> ([k], [AvailableTarget ()])
selectBuildableTargetsWith' p =
  (fmap . map) forgetTargetDetail . unzip . zipBuildableTargetsWith p

forgetTargetDetail :: AvailableTarget k -> AvailableTarget ()
forgetTargetDetail = fmap (const ())

forgetTargetsDetail :: [AvailableTarget k] -> [AvailableTarget ()]
forgetTargetsDetail = map forgetTargetDetail

-- | A basic @selectComponentTarget@ implementation to use or pass to
-- 'resolveTargets', that does the basic checks that the component is
-- buildable and isn't a test suite or benchmark that is disabled. This
-- can also be used to do these basic checks as part of a custom impl that
selectComponentTargetBasic
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either (TargetProblem a) k
selectComponentTargetBasic
  subtarget
  AvailableTarget
    { availableTargetPackageId = pkgid
    , availableTargetComponentName = cname
    , availableTargetStatus
    } =
    case availableTargetStatus of
      TargetDisabledByUser ->
        Left (TargetOptionalStanzaDisabledByUser pkgid cname subtarget)
      TargetDisabledBySolver ->
        Left (TargetOptionalStanzaDisabledBySolver pkgid cname subtarget)
      TargetNotLocal ->
        Left (TargetComponentNotProjectLocal pkgid cname subtarget)
      TargetNotBuildable ->
        Left (TargetComponentNotBuildable pkgid cname subtarget)
      TargetBuildable targetKey _ ->
        Right targetKey

-- | Wrapper around 'ProjectPlanning.pruneInstallPlanToTargets' that adjusts
-- for the extra unneeded info in the 'TargetsMap'.
pruneInstallPlanToTargets
  :: TargetAction
  -> TargetsMap
  -> ElaboratedInstallPlan
  -> ElaboratedInstallPlan
pruneInstallPlanToTargets targetActionType targetsMap elaboratedPlan =
  assert (Map.size targetsMap > 0) $
    ProjectPlanning.pruneInstallPlanToTargets
      targetActionType
      (Map.map (map fst) targetsMap)
      elaboratedPlan

-- | Utility used by repl and run to check if the targets spans multiple
-- components, since those commands do not support multiple components.
distinctTargetComponents :: TargetsMap -> Set.Set (UnitId, ComponentName)
distinctTargetComponents targetsMap =
  Set.fromList
    [ (uid, cname)
    | (uid, cts) <- Map.toList targetsMap
    , (ComponentTarget cname _, _) <- cts
    ]

------------------------------------------------------------------------------
-- Displaying what we plan to do
--

-- | Print a user-oriented presentation of the install plan, indicating what
-- will be built.
printPlan
  :: Verbosity
  -> ProjectBaseContext
  -> ProjectBuildContext
  -> IO ()
printPlan
  verbosity
  ProjectBaseContext
    { buildSettings = BuildTimeSettings{buildSettingDryRun}
    , projectConfig =
      ProjectConfig
        { projectConfigAllPackages =
          PackageConfig{packageConfigOptimization = globalOptimization}
        , projectConfigLocalPackages =
          PackageConfig{packageConfigOptimization = localOptimization}
        }
    , currentCommand
    }
  ProjectBuildContext
    { elaboratedPlanToExecute = elaboratedPlan
    , elaboratedShared
    , pkgsBuildStatus
    }
    | null pkgs && currentCommand == BuildCommand =
        notice verbosity "Up to date"
    | not (null pkgs) =
        noticeNoWrap verbosity $
          unlines $
            ( showBuildProfile
                ++ "In order, the following "
                ++ wouldWill
                ++ " be built"
                ++ ifNormal " (use -v for more details)"
                ++ ":"
            )
              : map showPkgAndReason pkgs
    | otherwise = return ()
    where
      pkgs = InstallPlan.executionOrder elaboratedPlan

      ifVerbose s
        | verbosity >= verbose = s
        | otherwise = ""

      ifNormal s
        | verbosity >= verbose = ""
        | otherwise = s

      wouldWill
        | buildSettingDryRun = "would"
        | otherwise = "will"

      showPkgAndReason :: ElaboratedReadyPackage -> String
      showPkgAndReason (ReadyPackage elab) =
        unwords $
          filter (not . null) $
            [ " -"
            , if verbosity >= deafening
                then prettyShow (installedUnitId elab)
                else prettyShow (packageId elab)
            , case elabBuildStyle elab of
                BuildInplaceOnly InMemory -> "(interactive)"
                _ -> ""
            , case elabPkgOrComp elab of
                ElabPackage pkg -> showTargets elab ++ ifVerbose (showStanzas (pkgStanzasEnabled pkg))
                ElabComponent comp ->
                  "(" ++ showComp elab comp ++ ")"
            , showFlagAssignment (nonDefaultFlags elab)
            , showConfigureFlags elab
            , let buildStatus = pkgsBuildStatus Map.! installedUnitId elab
               in "(" ++ showBuildStatus buildStatus ++ ")"
            ]

      showComp :: ElaboratedConfiguredPackage -> ElaboratedComponent -> String
      showComp elab comp =
        maybe "custom" prettyShow (compComponentName comp)
          ++ if Map.null (elabInstantiatedWith elab)
            then ""
            else
              " with "
                ++ intercalate
                  ", "
                  -- TODO: Abbreviate the UnitIds
                  [ prettyShow k ++ "=" ++ prettyShow v
                  | (k, v) <- Map.toList (elabInstantiatedWith elab)
                  ]

      nonDefaultFlags :: ElaboratedConfiguredPackage -> FlagAssignment
      nonDefaultFlags elab =
        elabFlagAssignment elab `diffFlagAssignment` elabFlagDefaults elab

      showTargets :: ElaboratedConfiguredPackage -> String
      showTargets elab
        | null (elabBuildTargets elab) = ""
        | otherwise =
            "("
              ++ intercalate
                ", "
                [ showComponentTarget (packageId elab) t
                | t <- elabBuildTargets elab
                ]
              ++ ")"

      showConfigureFlags :: ElaboratedConfiguredPackage -> String
      showConfigureFlags elab =
        let fullConfigureFlags =
              setupHsConfigureFlags
                (ReadyPackage elab)
                elaboratedShared
                verbosity
                "$builddir"
            -- \| Given a default value @x@ for a flag, nub @Flag x@
            -- into @NoFlag@.  This gives us a tidier command line
            -- rendering.
            nubFlag :: Eq a => a -> Setup.Flag a -> Setup.Flag a
            nubFlag x (Setup.Flag x') | x == x' = Setup.NoFlag
            nubFlag _ f = f

            (tryLibProfiling, tryExeProfiling) =
              computeEffectiveProfiling fullConfigureFlags

            partialConfigureFlags =
              mempty
                { configProf =
                    nubFlag False (configProf fullConfigureFlags)
                , configProfExe =
                    nubFlag tryExeProfiling (configProfExe fullConfigureFlags)
                , configProfLib =
                    nubFlag tryLibProfiling (configProfLib fullConfigureFlags)
                    -- Maybe there are more we can add
                }
         in -- Not necessary to "escape" it, it's just for user output
            unwords . ("" :) $
              commandShowOptions
                (Setup.configureCommand (pkgConfigCompilerProgs elaboratedShared))
                partialConfigureFlags

      showBuildStatus :: BuildStatus -> String
      showBuildStatus status = case status of
        BuildStatusPreExisting -> "existing package"
        BuildStatusInstalled -> "already installed"
        BuildStatusDownload{} -> "requires download & build"
        BuildStatusUnpack{} -> "requires build"
        BuildStatusRebuild _ rebuild -> case rebuild of
          BuildStatusConfigure
            (MonitoredValueChanged _) -> "configuration changed"
          BuildStatusConfigure mreason -> showMonitorChangedReason mreason
          BuildStatusBuild _ buildreason -> case buildreason of
            BuildReasonDepsRebuilt -> "dependency rebuilt"
            BuildReasonFilesChanged
              mreason -> showMonitorChangedReason mreason
            BuildReasonExtraTargets _ -> "additional components to build"
            BuildReasonEphemeralTargets -> "ephemeral targets"
        BuildStatusUpToDate{} -> "up to date" -- doesn't happen
      showMonitorChangedReason :: MonitorChangedReason a -> String
      showMonitorChangedReason (MonitoredFileChanged file) =
        "file " ++ file ++ " changed"
      showMonitorChangedReason (MonitoredValueChanged _) = "value changed"
      showMonitorChangedReason MonitorFirstRun = "first run"
      showMonitorChangedReason MonitorCorruptCache =
        "cannot read state cache"

      showBuildProfile :: String
      showBuildProfile =
        "Build profile: "
          ++ unwords
            [ "-w " ++ (showCompilerId . pkgConfigCompiler) elaboratedShared
            , "-O"
                ++ ( case globalOptimization <> localOptimization of -- if local is not set, read global
                      Setup.Flag NoOptimisation -> "0"
                      Setup.Flag NormalOptimisation -> "1"
                      Setup.Flag MaximumOptimisation -> "2"
                      Setup.NoFlag -> "1"
                   )
            ]
          ++ "\n"

writeBuildReports :: BuildTimeSettings -> ProjectBuildContext -> ElaboratedInstallPlan -> BuildOutcomes -> IO ()
writeBuildReports settings buildContext plan buildOutcomes = do
  let plat@(Platform arch os) = pkgConfigPlatform . elaboratedShared $ buildContext
      comp = pkgConfigCompiler . elaboratedShared $ buildContext
      getRepo (RepoTarballPackage r _ _) = Just r
      getRepo _ = Nothing
      fromPlanPackage (InstallPlan.Configured pkg) (Just result) =
        let installOutcome = case result of
              Left bf -> case buildFailureReason bf of
                GracefulFailure _ -> BuildReports.PlanningFailed
                DependentFailed p -> BuildReports.DependencyFailed p
                DownloadFailed _ -> BuildReports.DownloadFailed
                UnpackFailed _ -> BuildReports.UnpackFailed
                ConfigureFailed _ -> BuildReports.ConfigureFailed
                BuildFailed _ -> BuildReports.BuildFailed
                TestsFailed _ -> BuildReports.TestsFailed
                InstallFailed _ -> BuildReports.InstallFailed
                ReplFailed _ -> BuildReports.InstallOk
                HaddocksFailed _ -> BuildReports.InstallOk
                BenchFailed _ -> BuildReports.InstallOk
              Right _br -> BuildReports.InstallOk

            docsOutcome = case result of
              Left bf -> case buildFailureReason bf of
                HaddocksFailed _ -> BuildReports.Failed
                _ -> BuildReports.NotTried
              Right br -> case buildResultDocs br of
                DocsNotTried -> BuildReports.NotTried
                DocsFailed -> BuildReports.Failed
                DocsOk -> BuildReports.Ok

            testsOutcome = case result of
              Left bf -> case buildFailureReason bf of
                TestsFailed _ -> BuildReports.Failed
                _ -> BuildReports.NotTried
              Right br -> case buildResultTests br of
                TestsNotTried -> BuildReports.NotTried
                TestsOk -> BuildReports.Ok
         in Just $ (BuildReports.BuildReport (packageId pkg) os arch (compilerId comp) cabalInstallID (elabFlagAssignment pkg) (map (packageId . fst) $ elabLibDependencies pkg) installOutcome docsOutcome testsOutcome, getRepo . elabPkgSourceLocation $ pkg) -- TODO handle failure log files?
      fromPlanPackage _ _ = Nothing
      buildReports = mapMaybe (\x -> fromPlanPackage x (InstallPlan.lookupBuildOutcome x buildOutcomes)) $ InstallPlan.toList plan

  BuildReports.storeLocal
    (compilerInfo comp)
    (buildSettingSummaryFile settings)
    buildReports
    plat

-- Note this doesn't handle the anonymous build reports set by buildSettingBuildReports but those appear to not be used or missed from v1
-- The usage pattern appears to be that rather than rely on flags to cabal to send build logs to the right place and package them with reports, etc, it is easier to simply capture its output to an appropriate handle.

-- | If there are build failures then report them and throw an exception.
dieOnBuildFailures
  :: Verbosity
  -> CurrentCommand
  -> ElaboratedInstallPlan
  -> BuildOutcomes
  -> IO ()
dieOnBuildFailures verbosity currentCommand plan buildOutcomes
  | null failures = return ()
  | isSimpleCase = exitFailure
  | otherwise = do
      -- For failures where we have a build log, print the log plus a header
      sequence_
        [ do
          notice verbosity $
            '\n'
              : renderFailureDetail False pkg reason
              ++ "\nBuild log ( "
              ++ logfile
              ++ " ):"
          readFile logfile >>= noticeNoWrap verbosity
        | (pkg, ShowBuildSummaryAndLog reason logfile) <-
            failuresClassification
        ]

      -- For all failures, print either a short summary (if we showed the
      -- build log) or all details
      dieIfNotHaddockFailure verbosity $
        unlines
          [ case failureClassification of
            ShowBuildSummaryAndLog reason _
              | verbosity > normal ->
                  renderFailureDetail mentionDepOf pkg reason
              | otherwise ->
                  renderFailureSummary mentionDepOf pkg reason
                    ++ ". See the build log above for details."
            ShowBuildSummaryOnly reason ->
              renderFailureDetail mentionDepOf pkg reason
          | let mentionDepOf = verbosity <= normal
          , (pkg, failureClassification) <- failuresClassification
          ]
  where
    failures :: [(UnitId, BuildFailure)]
    failures =
      [ (pkgid, failure)
      | (pkgid, Left failure) <- Map.toList buildOutcomes
      ]

    failuresClassification :: [(ElaboratedConfiguredPackage, BuildFailurePresentation)]
    failuresClassification =
      [ (pkg, classifyBuildFailure failure)
      | (pkgid, failure) <- failures
      , case buildFailureReason failure of
          DependentFailed{} -> verbosity > normal
          _ -> True
      , InstallPlan.Configured pkg <-
          maybeToList (InstallPlan.lookup plan pkgid)
      ]

    dieIfNotHaddockFailure :: Verbosity -> String -> IO ()
    dieIfNotHaddockFailure
      | currentCommand == HaddockCommand = die'
      | all isHaddockFailure failuresClassification = warn
      | otherwise = die'
      where
        isHaddockFailure
          (_, ShowBuildSummaryOnly (HaddocksFailed _)) = True
        isHaddockFailure
          (_, ShowBuildSummaryAndLog (HaddocksFailed _) _) = True
        isHaddockFailure
          _ = False

    classifyBuildFailure :: BuildFailure -> BuildFailurePresentation
    classifyBuildFailure
      BuildFailure
        { buildFailureReason = reason
        , buildFailureLogFile = mlogfile
        } =
        maybe
          (ShowBuildSummaryOnly reason)
          (ShowBuildSummaryAndLog reason)
          $ do
            logfile <- mlogfile
            e <- buildFailureException reason
            ExitFailure 1 <- fromException e
            return logfile

    -- Special case: we don't want to report anything complicated in the case
    -- of just doing build on the current package, since it's clear from
    -- context which package failed.
    --
    -- We generalise this rule as follows:
    --  - if only one failure occurs, and it is in a single root
    --    package (i.e. a package with nothing else depending on it)
    --  - and that failure is of a kind that always reports enough
    --    detail itself (e.g. ghc reporting errors on stdout)
    --  - then we do not report additional error detail or context.
    --
    isSimpleCase :: Bool
    isSimpleCase
      | [(pkgid, failure)] <- failures
      , [pkg] <- rootpkgs
      , installedUnitId pkg == pkgid
      , isFailureSelfExplanatory (buildFailureReason failure)
      , currentCommand `notElem` [InstallCommand, BuildCommand, ReplCommand] =
          True
      | otherwise =
          False

    -- NB: if the Setup script segfaulted or was interrupted,
    -- we should give more detailed information.  So only
    -- assume that exit code 1 is "pedestrian failure."
    isFailureSelfExplanatory :: BuildFailureReason -> Bool
    isFailureSelfExplanatory (BuildFailed e)
      | Just (ExitFailure 1) <- fromException e = True
    isFailureSelfExplanatory (ConfigureFailed e)
      | Just (ExitFailure 1) <- fromException e = True
    isFailureSelfExplanatory _ = False

    rootpkgs :: [ElaboratedConfiguredPackage]
    rootpkgs =
      [ pkg
      | InstallPlan.Configured pkg <- InstallPlan.toList plan
      , hasNoDependents pkg
      ]

    ultimateDeps
      :: UnitId
      -> [InstallPlan.GenericPlanPackage InstalledPackageInfo ElaboratedConfiguredPackage]
    ultimateDeps pkgid =
      filter
        (\pkg -> hasNoDependents pkg && installedUnitId pkg /= pkgid)
        (InstallPlan.reverseDependencyClosure plan [pkgid])

    hasNoDependents :: HasUnitId pkg => pkg -> Bool
    hasNoDependents = null . InstallPlan.revDirectDeps plan . installedUnitId

    renderFailureDetail :: Bool -> ElaboratedConfiguredPackage -> BuildFailureReason -> String
    renderFailureDetail mentionDepOf pkg reason =
      renderFailureSummary mentionDepOf pkg reason
        ++ "."
        ++ renderFailureExtraDetail reason
        ++ maybe "" showException (buildFailureException reason)

    renderFailureSummary :: Bool -> ElaboratedConfiguredPackage -> BuildFailureReason -> String
    renderFailureSummary mentionDepOf pkg reason =
      case reason of
        DownloadFailed _ -> "Failed to download " ++ pkgstr
        UnpackFailed _ -> "Failed to unpack " ++ pkgstr
        ConfigureFailed _ -> "Failed to build " ++ pkgstr
        BuildFailed _ -> "Failed to build " ++ pkgstr
        ReplFailed _ -> "repl failed for " ++ pkgstr
        HaddocksFailed _ -> "Failed to build documentation for " ++ pkgstr
        TestsFailed _ -> "Tests failed for " ++ pkgstr
        BenchFailed _ -> "Benchmarks failed for " ++ pkgstr
        InstallFailed _ -> "Failed to build " ++ pkgstr
        GracefulFailure msg -> msg
        DependentFailed depid ->
          "Failed to build "
            ++ prettyShow (packageId pkg)
            ++ " because it depends on "
            ++ prettyShow depid
            ++ " which itself failed to build"
      where
        pkgstr =
          elabConfiguredName verbosity pkg
            ++ if mentionDepOf
              then renderDependencyOf (installedUnitId pkg)
              else ""

    renderFailureExtraDetail :: BuildFailureReason -> String
    renderFailureExtraDetail (ConfigureFailed _) =
      " The failure occurred during the configure step."
    renderFailureExtraDetail (InstallFailed _) =
      " The failure occurred during the final install step."
    renderFailureExtraDetail _ =
      ""

    renderDependencyOf :: UnitId -> String
    renderDependencyOf pkgid =
      case ultimateDeps pkgid of
        [] -> ""
        (p1 : []) ->
          " (which is required by " ++ elabPlanPackageName verbosity p1 ++ ")"
        (p1 : p2 : []) ->
          " (which is required by "
            ++ elabPlanPackageName verbosity p1
            ++ " and "
            ++ elabPlanPackageName verbosity p2
            ++ ")"
        (p1 : p2 : _) ->
          " (which is required by "
            ++ elabPlanPackageName verbosity p1
            ++ ", "
            ++ elabPlanPackageName verbosity p2
            ++ " and others)"

    showException e = case fromException e of
      Just (ExitFailure 1) -> ""

{- FOURMOLU_DISABLE -}
#ifdef MIN_VERSION_unix
      -- Note [Positive "signal" exit code]
      -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- What's the business with the test for negative and positive
      -- signal values?  The API for process specifies that if the
      -- process died due to a signal, it returns a *negative* exit
      -- code.  So that's the negative test.
      --
      -- What about the positive test?  Well, when we find out that
      -- a process died due to a signal, we ourselves exit with that
      -- exit code.  However, we don't "kill ourselves" with the
      -- signal; we just exit with the same code as the signal: thus
      -- the caller sees a *positive* exit code.  So that's what
      -- happens when we get a positive exit code.
      Just (ExitFailure n)
        | -n == fromIntegral sigSEGV ->
            " The build process segfaulted (i.e. SIGSEGV)."

        |  n == fromIntegral sigSEGV ->
            " The build process terminated with exit code " ++ show n
         ++ " which may be because some part of it segfaulted. (i.e. SIGSEGV)."

        | -n == fromIntegral sigKILL ->
            " The build process was killed (i.e. SIGKILL). " ++ explanation

        |  n == fromIntegral sigKILL ->
            " The build process terminated with exit code " ++ show n
         ++ " which may be because some part of it was killed "
         ++ "(i.e. SIGKILL). " ++ explanation
        where
          explanation =
            "The typical reason for this is that there is not "
            ++ "enough memory available (e.g. the OS killed a process "
            ++ "using lots of memory)."
#endif
      Just (ExitFailure n) ->
        " The build process terminated with exit code " ++ show n

      _ -> " The exception was:\n  "
#if MIN_VERSION_base(4,8,0)
             ++ displayException e
#else
             ++ show e
#endif

    buildFailureException :: BuildFailureReason -> Maybe SomeException
    buildFailureException reason =
      case reason of
        DownloadFailed  e -> Just e
        UnpackFailed    e -> Just e
        ConfigureFailed e -> Just e
        BuildFailed     e -> Just e
        ReplFailed      e -> Just e
        HaddocksFailed  e -> Just e
        TestsFailed     e -> Just e
        BenchFailed     e -> Just e
        InstallFailed   e -> Just e
        GracefulFailure _ -> Nothing
        DependentFailed _ -> Nothing
{- FOURMOLU_ENABLE -}

data BuildFailurePresentation
  = ShowBuildSummaryOnly BuildFailureReason
  | ShowBuildSummaryAndLog BuildFailureReason FilePath

-------------------------------------------------------------------------------
-- Dummy projects
-------------------------------------------------------------------------------

-- | Create a dummy project context, without a .cabal or a .cabal.project file
-- (a place where to put a temporary dist directory is still needed)
establishDummyProjectBaseContext
  :: Verbosity
  -> ProjectConfig
  -- ^ Project configuration including the global config if needed
  -> DistDirLayout
  -- ^ Where to put the dist directory
  -> [PackageSpecifier UnresolvedSourcePackage]
  -- ^ The packages to be included in the project
  -> CurrentCommand
  -> IO ProjectBaseContext
establishDummyProjectBaseContext verbosity projectConfig distDirLayout localPackages currentCommand = do
  let ProjectConfigBuildOnly
        { projectConfigLogsDir
        } = projectConfigBuildOnly projectConfig

      ProjectConfigShared
        { projectConfigStoreDir
        } = projectConfigShared projectConfig

      mlogsDir = flagToMaybe projectConfigLogsDir
      mstoreDir = flagToMaybe projectConfigStoreDir

  cabalDirLayout <- mkCabalDirLayout mstoreDir mlogsDir

  let buildSettings :: BuildTimeSettings
      buildSettings =
        resolveBuildTimeSettings
          verbosity
          cabalDirLayout
          projectConfig
      installedPackages = Nothing

  return
    ProjectBaseContext
      { distDirLayout
      , cabalDirLayout
      , projectConfig
      , localPackages
      , buildSettings
      , currentCommand
      , installedPackages
      }

establishDummyDistDirLayout :: Verbosity -> ProjectConfig -> FilePath -> IO DistDirLayout
establishDummyDistDirLayout verbosity cliConfig tmpDir = do
  let distDirLayout = defaultDistDirLayout projectRoot mdistDirectory Nothing

  -- Create the dist directories
  createDirectoryIfMissingVerbose verbosity True $ distDirectory distDirLayout
  createDirectoryIfMissingVerbose verbosity True $ distProjectCacheDirectory distDirLayout

  return distDirLayout
  where
    mdistDirectory =
      flagToMaybe $
        projectConfigDistDir $
          projectConfigShared cliConfig
    projectRoot = ProjectRootImplicit tmpDir
