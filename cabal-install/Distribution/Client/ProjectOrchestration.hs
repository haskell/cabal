{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

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
--
module Distribution.Client.ProjectOrchestration (
    -- * Pre-build phase: decide what to do.
    runProjectPreBuildPhase,
    CliConfigFlags,
    PreBuildHooks(..),
    ProjectBuildContext(..),

    -- ** Adjusting the plan
    resolveTargets,
    BuildTarget,
    PackageId,
    AvailableTarget(..),
    AvailableTargetStatus(..),
    TargetRequested(..),
    ComponentName(..),
    ComponentTarget(..),
    TargetProblem,
    showTargetProblem,
    selectComponentTargetBasic,

    pruneInstallPlanToTargets,
    TargetAction(..),
    pruneInstallPlanToDependencies,
    elaboratePackageTargets,
    printPlan,

    -- * Build phase: now do it.
    runProjectBuildPhase,

    -- * Post build actions
    runProjectPostBuildPhase,
    dieOnBuildFailures,
  ) where

import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanning
import           Distribution.Client.ProjectPlanning.Types
import           Distribution.Client.ProjectBuilding
import           Distribution.Client.ProjectPlanOutput

import           Distribution.Client.Types
                   ( GenericReadyPackage(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.BuildTarget
                   ( UserBuildTarget, resolveUserBuildTargets
                   , BuildTarget(..), buildTargetPackage )
import           Distribution.Client.DistDirLayout
import           Distribution.Client.Config (defaultCabalDir)
import           Distribution.Client.Setup hiding (packageName)

import           Distribution.Solver.Types.OptionalStanza

import           Distribution.Package
                   hiding (InstalledPackageId, installedPackageId)
import           Distribution.PackageDescription (FlagAssignment, showFlagValue)
import           Distribution.Simple.LocalBuildInfo
                   ( ComponentName(..) )
import           Distribution.Simple.Setup (HaddockFlags)
import qualified Distribution.Simple.Setup as Setup
import           Distribution.Simple.Command (commandShowOptions)

import           Distribution.Simple.Utils
                   ( die', info
                   , notice, noticeNoWrap, debugNoWrap )
import           Distribution.Verbosity
import           Distribution.Text

import qualified Data.Monoid as Mon
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.List
import           Data.Maybe
import           Data.Either
import           Control.Exception (Exception(..))
import           System.Exit (ExitCode(..), exitFailure)
import qualified System.Process.Internals as Process (translate)
#ifdef MIN_VERSION_unix
import           System.Posix.Signals (sigKILL, sigSEGV)
#endif


-- | Command line configuration flags. These are used to extend\/override the
-- project configuration.
--
type CliConfigFlags = ( GlobalFlags
                      , ConfigFlags, ConfigExFlags
                      , InstallFlags, HaddockFlags )

-- | Hooks to alter the behaviour of 'runProjectPreBuildPhase'.
--
-- For example the @configure@, @build@ and @repl@ commands use this to get
-- their different behaviour.
--
data PreBuildHooks = PreBuildHooks {
       hookPrePlanning      :: FilePath
                            -> DistDirLayout
                            -> ProjectConfig
                            -> IO (),
       hookSelectPlanSubset :: BuildTimeSettings
                            -> ElaboratedInstallPlan
                            -> IO ElaboratedInstallPlan
     }

-- | This holds the context between the pre-build, build and post-build phases.
--
data ProjectBuildContext = ProjectBuildContext {
      projectRootDir         :: FilePath,
      distDirLayout          :: DistDirLayout,

      -- | This is the improved plan, before we select a plan subset based on
      -- the build targets, and before we do the dry-run. So this contains
      -- all packages in the project.
      elaboratedPlanOriginal :: ElaboratedInstallPlan,

      -- | This is the 'elaboratedPlanOriginal' after we select a plan subset
      -- and do the dry-run phase to find out what is up-to or out-of date.
      -- This is the plan that will be executed during the build phase. So
      -- this contains only a subset of packages in the project.
      elaboratedPlanToExecute:: ElaboratedInstallPlan,

      -- | The part of the install plan that's shared between all packages in
      -- the plan. This does not change between the two plan variants above,
      -- so there is just the one copy.
      elaboratedShared       :: ElaboratedSharedConfig,

      -- | The result of the dry-run phase. This tells us about each member of
      -- the 'elaboratedPlanToExecute'.
      pkgsBuildStatus        :: BuildStatusMap,

      buildSettings          :: BuildTimeSettings
    }


-- | Pre-build phase: decide what to do.
--
runProjectPreBuildPhase :: Verbosity
                        -> CliConfigFlags
                        -> PreBuildHooks
                        -> IO ProjectBuildContext
runProjectPreBuildPhase
    verbosity
    ( globalFlags
    , configFlags, configExFlags
    , installFlags, haddockFlags )
    PreBuildHooks{..} = do

    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- findProjectRoot installFlags
    let distDirLayout = defaultDistDirLayout configFlags projectRootDir

    let cliConfig = commandLineFlagsToProjectConfig
                      globalFlags configFlags configExFlags
                      installFlags haddockFlags

    hookPrePlanning
      projectRootDir
      distDirLayout
      cliConfig

    -- Take the project configuration and make a plan for how to build
    -- everything in the project. This is independent of any specific targets
    -- the user has asked for.
    --
    (elaboratedPlan, _, elaboratedShared, projectConfig) <-
      rebuildInstallPlan verbosity installFlags
                         projectRootDir distDirLayout cabalDirLayout
                         cliConfig

    let buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          (projectConfigShared    projectConfig)
                          (projectConfigBuildOnly projectConfig)
                          (projectConfigBuildOnly cliConfig)
    info verbosity $ "Number of threads used: "
      ++ (show . buildSettingNumJobs $ buildSettings) ++ "."
    -- The plan for what to do is represented by an 'ElaboratedInstallPlan'

    -- Now given the specific targets the user has asked for, decide
    -- which bits of the plan we will want to execute.
    --
    elaboratedPlan' <- hookSelectPlanSubset buildSettings elaboratedPlan

    -- Check which packages need rebuilding.
    -- This also gives us more accurate reasons for the --dry-run output.
    --
    pkgsBuildStatus <- rebuildTargetsDryRun distDirLayout elaboratedShared
                                            elaboratedPlan'

    -- Improve the plan by marking up-to-date packages as installed.
    --
    let elaboratedPlan'' = improveInstallPlanWithUpToDatePackages
                             pkgsBuildStatus elaboratedPlan'
    debugNoWrap verbosity (InstallPlan.showInstallPlan elaboratedPlan'')

    return ProjectBuildContext {
      projectRootDir,
      distDirLayout,
      elaboratedPlanOriginal = elaboratedPlan,
      elaboratedPlanToExecute  = elaboratedPlan'',
      elaboratedShared,
      pkgsBuildStatus,
      buildSettings
    }


-- | Build phase: now do it.
--
-- Execute all or parts of the description of what to do to build or
-- rebuild the various packages needed.
--
runProjectBuildPhase :: Verbosity
                     -> ProjectBuildContext
                     -> IO BuildOutcomes
runProjectBuildPhase _ ProjectBuildContext {buildSettings}
  | buildSettingDryRun buildSettings
  = return Map.empty

runProjectBuildPhase verbosity ProjectBuildContext {..} =
    fmap (Map.union (previousBuildOutcomes pkgsBuildStatus)) $
    rebuildTargets verbosity
                   distDirLayout
                   elaboratedPlanToExecute
                   elaboratedShared
                   pkgsBuildStatus
                   buildSettings
  where
    previousBuildOutcomes :: BuildStatusMap -> BuildOutcomes
    previousBuildOutcomes =
      Map.mapMaybe $ \status -> case status of
        BuildStatusUpToDate buildSuccess -> Just (Right buildSuccess)
        --TODO: [nice to have] record build failures persistently
        _                                  -> Nothing

-- | Post-build phase: various administrative tasks
--
-- Update bits of state based on the build outcomes and report any failures.
--
runProjectPostBuildPhase :: Verbosity
                         -> ProjectBuildContext
                         -> BuildOutcomes
                         -> IO ()
runProjectPostBuildPhase _ ProjectBuildContext {buildSettings} _
  | buildSettingDryRun buildSettings
  = return ()

runProjectPostBuildPhase verbosity ProjectBuildContext {..} buildOutcomes = do
    -- Update other build artefacts
    -- TODO: currently none, but could include:
    --        - bin symlinks/wrappers
    --        - haddock/hoogle/ctags indexes
    --        - delete stale lib registrations
    --        - delete stale package dirs

    postBuildStatus <- updatePostBuildProjectStatus
                         verbosity
                         distDirLayout
                         elaboratedPlanOriginal
                         pkgsBuildStatus
                         buildOutcomes

    writePlanGhcEnvironment projectRootDir
                            elaboratedPlanOriginal
                            elaboratedShared
                            postBuildStatus

    -- Finally if there were any build failures then report them and throw
    -- an exception to terminate the program
    dieOnBuildFailures verbosity elaboratedPlanToExecute buildOutcomes

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

-- | Adjust an 'ElaboratedInstallPlan' by selecting just those parts of it
-- required to build the given user targets.
--
-- How to get the 'PackageTarget's from the 'UserBuildTarget' is customisable,
-- so that we can change the meaning of @pkgname@ to target a build or
-- repl depending on which command is calling it.
--
-- Conceptually, every target identifies one or more roots in the
-- 'ElaboratedInstallPlan', which we then use to determine the closure
-- of what packages need to be built, dropping everything from
-- 'ElaboratedInstallPlan' that is unnecessary.
--
-- There is a complication, however: In an ideal world, every
-- possible target would be a node in the graph.  However, it is
-- currently not possible (and possibly not even desirable) to invoke a
-- Setup script to build *just* one file.  Similarly, it is not possible
-- to invoke a pre Cabal-1.25 custom Setup script and build only one
-- component.  In these cases, we want to build the entire package, BUT
-- only actually building some of the files/components.  This is what
-- 'pkgBuildTargets', 'pkgReplTarget' and 'pkgBuildHaddock' control.
-- Arguably, these should an out-of-band mechanism rather than stored
-- in 'ElaboratedInstallPlan', but it's what we have.  We have
-- to fiddle around with the ElaboratedConfiguredPackage roots to say
-- what it will build.
--
resolveTargets :: (forall k. BuildTarget PackageId -> [AvailableTarget k] -> Either e [k])
               -> (forall k. BuildTarget PackageId ->  AvailableTarget k  -> Either e  k )
               -> (TargetProblem -> e)
               -> ElaboratedInstallPlan
               -> [UserBuildTarget]
               -> IO (Either [e] (Map UnitId [ComponentTarget]))
resolveTargets selectPackageTargets selectComponentTarget liftProblem
               installPlan userBuildTargets = do

    -- Match the user targets against the available targets. If no targets are
    -- given this uses the package in the current directory, if any.
    --
    buildTargets <- resolveUserBuildTargets verbosity localPackages userBuildTargets
    --TODO: [required eventually] report something if there are no targets

    --TODO: [required eventually]
    -- we cannot resolve names of packages other than those that are
    -- directly in the current plan. We ought to keep a set of the known
    -- hackage packages so we can resolve names to those. Though we don't
    -- really need that until we can do something sensible with packages
    -- outside of the project.

    -- Now check if those targets belong to the current project or not.
    -- Ultimately we want to do something sensible for targets not in this
    -- project, but for now we just bail. This gives us back the ipkgid from
    -- the plan.
    --
    return $ resolveAndCheckTargets
               selectPackageTargets
               selectComponentTarget
               liftProblem
               installPlan
               buildTargets
  where
    localPackages =
      [ (elabPkgDescription elab, elabPkgSourceLocation elab)
      | InstallPlan.Configured elab <- InstallPlan.toList installPlan ]
      --TODO: [code cleanup] is there a better way to identify local packages?

    verbosity = normal --TODO: remove

resolveAndCheckTargets :: forall err.
                          (forall k. BuildTarget PackageId -> [AvailableTarget k] -> Either err [k])
                          -- ^ selectPackageTargets
                       -> (forall k.  BuildTarget PackageId -> AvailableTarget k  -> Either err  k )
                          -- ^ selectComponentTarget
                       -> (TargetProblem -> err)
                          -- ^ Lift a 'TargetProblem' to the error type
                       -> ElaboratedInstallPlan
                       -> [BuildTarget PackageId]
                       -> Either [err] (Map UnitId [ComponentTarget])
resolveAndCheckTargets selectPackageTargets selectComponentTarget liftProblem
                       installPlan targets =
    case partitionEithers (map checkTarget targets) of
      ([], targets') -> Right
                      . Map.map nubComponentTargets
                      $ Map.fromListWith (++)
                          [ (uid, [t]) | (uid, t) <- concat targets' ]
      (problems, _)  -> Left problems
  where
    -- TODO [required eventually] currently all build targets refer to packages
    -- inside the project. Ultimately this has to be generalised to allow
    -- referring to other packages and targets.
    checkTarget :: BuildTarget PackageId
                -> Either err [(UnitId, ComponentTarget)]

    -- We can ask to build any whole package, project-local or a dependency
    checkTarget bt@(BuildTargetPackage pkgid)
      | Just ats <- Map.lookup pkgid availableTargetsByPackage
      = case selectPackageTargets bt ats of
          Left  e  -> Left e
          Right ts -> Right [ (unitid, ComponentTarget cname WholeComponent)
                            | (unitid, cname) <- ts ]

      | otherwise
      = Left (liftProblem (TargetNotInProject (packageName pkgid)))

    checkTarget bt@(BuildTargetComponent pkgid cname) =
      checkComponentTarget bt pkgid cname WholeComponent

    checkTarget bt@(BuildTargetModule pkgid cname mn) =
      checkComponentTarget bt pkgid cname (ModuleTarget mn)

    checkTarget bt@(BuildTargetFile pkgid cname fn) =
      checkComponentTarget bt pkgid cname (FileTarget fn)

    checkComponentTarget bt pkgid cname subtarget
      | Just ats <- Map.lookup (pkgid, cname) availableTargetsByComponent
      = case partitionEithers (map (selectComponentTarget bt) ats) of
          (e:_,_) -> Left e
          ([],ts) -> Right [ (unitid, ctarget)
                           | let ctarget = ComponentTarget cname subtarget
                           , (unitid, _) <- ts ]

      | otherwise
      = Left (liftProblem (TargetNotInProject (packageName pkgid)))

    availableTargetsByPackage   :: Map PackageId                  [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByComponent :: Map (PackageId, ComponentName) [AvailableTarget (UnitId, ComponentName)]
    availableTargetsByPackage   = Map.mapKeysWith
                                    (++) (\(pkgid, _cname) -> pkgid)
                                    availableTargetsByComponent
    availableTargetsByComponent = availableTargets installPlan

    --TODO: [research required] what if the solution has multiple versions of this package?
    --      e.g. due to setup deps or due to multiple independent sets of
    --      packages being built (e.g. ghc + ghcjs in a project)

-- | A basic @selectComponentTarget@ implementation to use or pass to
-- 'resolveTargets', that does the basic checks that the component is
-- buildable and isn't a test suite or benchmark that is disabled. This
-- can also be used to do these basic checks as part of a custom impl that
--
selectComponentTargetBasic :: BuildTarget PackageId
                           -> AvailableTarget k
                           -> Either TargetProblem k
selectComponentTargetBasic buildTarget AvailableTarget{..} =
    case availableTargetStatus of
      TargetDisabledByUser ->
        Left (TargetOptionalStanzaDisabled True)

      TargetDisabledBySolver ->
        Left (TargetOptionalStanzaDisabled False)

      TargetNotLocal ->
        Left (TargetComponentNotProjectLocal (fmap packageName buildTarget))

      TargetNotBuildable ->
        Left (TargetComponentNotBuildable)

      TargetBuildable targetKey _ ->
        Right targetKey

data TargetProblem
   = TargetNotInProject PackageName
   | TargetComponentNotProjectLocal (BuildTarget PackageName)
   | TargetComponentNotBuildable
   | TargetOptionalStanzaDisabled Bool
      -- ^ @True@: explicitly disabled by user
      -- @False@: disabled by solver
  deriving (Eq, Show)

showTargetProblem :: TargetProblem -> String
showTargetProblem (TargetNotInProject pn) =
        "Cannot build the package " ++ display pn ++ ", it is not in this project."
     ++ "(either directly or indirectly). If you want to add it to the "
     ++ "project then edit the cabal.project file."

showTargetProblem (TargetComponentNotProjectLocal t) =
        "The package " ++ display (buildTargetPackage t) ++ " is in the "
     ++ "project but it is not a locally unpacked package, so  "

showTargetProblem _ = undefined


------------------------------------------------------------------------------
-- Displaying what we plan to do
--

-- | Print a user-oriented presentation of the install plan, indicating what
-- will be built.
--
printPlan :: Verbosity -> ProjectBuildContext -> IO ()
printPlan verbosity
          ProjectBuildContext {
            elaboratedPlanToExecute = elaboratedPlan,
            elaboratedShared,
            pkgsBuildStatus,
            buildSettings = BuildTimeSettings{buildSettingDryRun}
          }

  | null pkgs
  = notice verbosity "Up to date"

  | otherwise
  = noticeNoWrap verbosity $ unlines $
      ("In order, the following " ++ wouldWill ++ " be built" ++
      ifNormal " (use -v for more details)" ++ ":")
    : map showPkgAndReason pkgs

  where
    pkgs = InstallPlan.executionOrder elaboratedPlan

    ifVerbose s | verbosity >= verbose = s
                | otherwise            = ""

    ifNormal s | verbosity >= verbose = ""
               | otherwise            = s

    wouldWill | buildSettingDryRun = "would"
              | otherwise          = "will"

    showPkgAndReason :: ElaboratedReadyPackage -> String
    showPkgAndReason (ReadyPackage elab) =
      " - " ++
      (if verbosity >= deafening
        then display (installedUnitId elab)
        else display (packageId elab)
        ) ++
      (case elabPkgOrComp elab of
          ElabPackage pkg -> showTargets elab ++ ifVerbose (showStanzas pkg)
          ElabComponent comp ->
            " (" ++ showComp elab comp ++ ")"
            ) ++
      showFlagAssignment (nonDefaultFlags elab) ++
      showConfigureFlags elab ++
      let buildStatus = pkgsBuildStatus Map.! installedUnitId elab in
      " (" ++ showBuildStatus buildStatus ++ ")"

    showComp elab comp =
        maybe "custom" display (compComponentName comp) ++
        if Map.null (elabInstantiatedWith elab)
            then ""
            else " with " ++
                intercalate ", "
                    -- TODO: Abbreviate the UnitIds
                    [ display k ++ "=" ++ display v
                    | (k,v) <- Map.toList (elabInstantiatedWith elab) ]

    nonDefaultFlags :: ElaboratedConfiguredPackage -> FlagAssignment
    nonDefaultFlags elab = elabFlagAssignment elab \\ elabFlagDefaults elab

    showStanzas pkg = concat
                    $ [ " *test"
                      | TestStanzas  `Set.member` pkgStanzasEnabled pkg ]
                   ++ [ " *bench"
                      | BenchStanzas `Set.member` pkgStanzasEnabled pkg ]

    showTargets elab
      | null (elabBuildTargets elab) = ""
      | otherwise
      = " (" ++ intercalate ", " [ showComponentTarget (packageId elab) t | t <- elabBuildTargets elab ]
             ++ ")"

    showFlagAssignment :: FlagAssignment -> String
    showFlagAssignment = concatMap ((' ' :) . showFlagValue)

    showConfigureFlags elab =
        let fullConfigureFlags
              = setupHsConfigureFlags
                    (ReadyPackage elab)
                    elaboratedShared
                    verbosity
                    "$builddir"
            -- | Given a default value @x@ for a flag, nub @Flag x@
            -- into @NoFlag@.  This gives us a tidier command line
            -- rendering.
            nubFlag :: Eq a => a -> Setup.Flag a -> Setup.Flag a
            nubFlag x (Setup.Flag x') | x == x' = Setup.NoFlag
            nubFlag _ f = f
            -- TODO: Closely logic from 'configureProfiling'.
            tryExeProfiling = Setup.fromFlagOrDefault False
                                (configProf fullConfigureFlags)
            tryLibProfiling = Setup.fromFlagOrDefault False
                                (Mon.mappend (configProf    fullConfigureFlags)
                                             (configProfExe fullConfigureFlags))
            partialConfigureFlags
              = Mon.mempty {
                configProf    =
                    nubFlag False (configProf fullConfigureFlags),
                configProfExe =
                    nubFlag tryExeProfiling (configProfExe fullConfigureFlags),
                configProfLib =
                    nubFlag tryLibProfiling (configProfLib fullConfigureFlags)
                -- Maybe there are more we can add
              }
        in unwords . ("":) . map Process.translate $
            commandShowOptions
            (Setup.configureCommand (pkgConfigCompilerProgs elaboratedShared))
            partialConfigureFlags

    showBuildStatus status = case status of
      BuildStatusPreExisting -> "existing package"
      BuildStatusInstalled   -> "already installed"
      BuildStatusDownload {} -> "requires download & build"
      BuildStatusUnpack   {} -> "requires build"
      BuildStatusRebuild _ rebuild -> case rebuild of
        BuildStatusConfigure
          (MonitoredValueChanged _)   -> "configuration changed"
        BuildStatusConfigure mreason  -> showMonitorChangedReason mreason
        BuildStatusBuild _ buildreason -> case buildreason of
          BuildReasonDepsRebuilt      -> "dependency rebuilt"
          BuildReasonFilesChanged
            mreason                   -> showMonitorChangedReason mreason
          BuildReasonExtraTargets _   -> "additional components to build"
          BuildReasonEphemeralTargets -> "ephemeral targets"
      BuildStatusUpToDate {} -> "up to date" -- doesn't happen

    showMonitorChangedReason (MonitoredFileChanged file) = "file " ++ file ++ " changed"
    showMonitorChangedReason (MonitoredValueChanged _)   = "value changed"
    showMonitorChangedReason  MonitorFirstRun     = "first run"
    showMonitorChangedReason  MonitorCorruptCache = "cannot read state cache"


-- | If there are build failures then report them and throw an exception.
--
dieOnBuildFailures :: Verbosity
                   -> ElaboratedInstallPlan -> BuildOutcomes -> IO ()
dieOnBuildFailures verbosity plan buildOutcomes
  | null failures = return ()

  | isSimpleCase  = exitFailure

  | otherwise = do
      -- For failures where we have a build log, print the log plus a header
       sequence_
         [ do notice verbosity $
                '\n' : renderFailureDetail False pkg reason
                    ++ "\nBuild log ( " ++ logfile ++ " ):"
              readFile logfile >>= noticeNoWrap verbosity
         | (pkg, ShowBuildSummaryAndLog reason logfile)
             <- failuresClassification
         ]

       -- For all failures, print either a short summary (if we showed the
       -- build log) or all details
       die' verbosity $ unlines
         [ case failureClassification of
             ShowBuildSummaryAndLog reason _
               | verbosity > normal
              -> renderFailureDetail mentionDepOf pkg reason

               | otherwise
              -> renderFailureSummary mentionDepOf pkg reason
              ++ ". See the build log above for details."

             ShowBuildSummaryOnly reason ->
               renderFailureDetail mentionDepOf pkg reason

         | let mentionDepOf = verbosity <= normal
         , (pkg, failureClassification) <- failuresClassification ]
  where
    failures =  [ (pkgid, failure)
                | (pkgid, Left failure) <- Map.toList buildOutcomes ]

    failuresClassification =
      [ (pkg, classifyBuildFailure failure)
      | (pkgid, failure) <- failures
      , case buildFailureReason failure of
          DependentFailed {} -> verbosity > normal
          _                  -> True
      , InstallPlan.Configured pkg <-
           maybeToList (InstallPlan.lookup plan pkgid)
      ]

    classifyBuildFailure :: BuildFailure -> BuildFailurePresentation
    classifyBuildFailure BuildFailure {
                           buildFailureReason  = reason,
                           buildFailureLogFile = mlogfile
                         } =
      maybe (ShowBuildSummaryOnly   reason)
            (ShowBuildSummaryAndLog reason) $ do
        logfile <- mlogfile
        e       <- buildFailureException reason
        ExitFailure 1 <- fromException e
        return logfile

    -- Special case: we don't want to report anything complicated in the case
    -- of just doing build on the current package, since it's clear from
    -- context which package failed.
    --
    -- We generalise this rule as follows:
    --  - if only one failure occurs, and it is in a single root package (ie a
    --    package with nothing else depending on it)
    --  - and that failure is of a kind that always reports enough detail
    --    itself (e.g. ghc reporting errors on stdout)
    --  - then we do not report additional error detail or context.
    --
    isSimpleCase
      | [(pkgid, failure)] <- failures
      , [pkg]              <- rootpkgs
      , installedUnitId pkg == pkgid
      , isFailureSelfExplanatory (buildFailureReason failure)
      = True
      | otherwise
      = False

    -- NB: if the Setup script segfaulted or was interrupted,
    -- we should give more detailed information.  So only
    -- assume that exit code 1 is "pedestrian failure."
    isFailureSelfExplanatory (BuildFailed e)
      | Just (ExitFailure 1) <- fromException e = True

    isFailureSelfExplanatory (ConfigureFailed e)
      | Just (ExitFailure 1) <- fromException e = True

    isFailureSelfExplanatory _                  = False

    rootpkgs =
      [ pkg
      | InstallPlan.Configured pkg <- InstallPlan.toList plan
      , hasNoDependents pkg ]

    ultimateDeps pkgid =
        filter (\pkg -> hasNoDependents pkg && installedUnitId pkg /= pkgid)
               (InstallPlan.reverseDependencyClosure plan [pkgid])

    hasNoDependents :: HasUnitId pkg => pkg -> Bool
    hasNoDependents = null . InstallPlan.revDirectDeps plan . installedUnitId

    renderFailureDetail mentionDepOf pkg reason =
        renderFailureSummary mentionDepOf pkg reason ++ "."
     ++ renderFailureExtraDetail reason
     ++ maybe "" showException (buildFailureException reason)

    renderFailureSummary mentionDepOf pkg reason =
        case reason of
          DownloadFailed  _ -> "Failed to download " ++ pkgstr
          UnpackFailed    _ -> "Failed to unpack "   ++ pkgstr
          ConfigureFailed _ -> "Failed to build "    ++ pkgstr
          BuildFailed     _ -> "Failed to build "    ++ pkgstr
          ReplFailed      _ -> "repl failed for "    ++ pkgstr
          HaddocksFailed  _ -> "Failed to build documentation for " ++ pkgstr
          TestsFailed     _ -> "Tests failed for " ++ pkgstr
          InstallFailed   _ -> "Failed to build "  ++ pkgstr
          DependentFailed depid
                            -> "Failed to build " ++ display (packageId pkg)
                            ++ " because it depends on " ++ display depid
                            ++ " which itself failed to build"
      where
        pkgstr = elabConfiguredName verbosity pkg
              ++ if mentionDepOf
                   then renderDependencyOf (installedUnitId pkg)
                   else ""

    renderFailureExtraDetail reason =
      case reason of
        ConfigureFailed _ -> " The failure occurred during the configure step."
        InstallFailed   _ -> " The failure occurred during the final install step."
        _                 -> ""

    renderDependencyOf pkgid =
      case ultimateDeps pkgid of
        []         -> ""
        (p1:[])    -> " (which is required by " ++ elabPlanPackageName verbosity p1 ++ ")"
        (p1:p2:[]) -> " (which is required by " ++ elabPlanPackageName verbosity p1
                                     ++ " and " ++ elabPlanPackageName verbosity p2 ++ ")"
        (p1:p2:_)  -> " (which is required by " ++ elabPlanPackageName verbosity p1
                                        ++ ", " ++ elabPlanPackageName verbosity p2
                                        ++ " and others)"

    showException e = case fromException e of
      Just (ExitFailure 1) -> ""

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
          explanation = "The typical reason for this is that there is not "
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

    buildFailureException reason =
      case reason of
        DownloadFailed  e -> Just e
        UnpackFailed    e -> Just e
        ConfigureFailed e -> Just e
        BuildFailed     e -> Just e
        ReplFailed      e -> Just e
        HaddocksFailed  e -> Just e
        TestsFailed     e -> Just e
        InstallFailed   e -> Just e
        DependentFailed _ -> Nothing

data BuildFailurePresentation =
       ShowBuildSummaryOnly   BuildFailureReason
     | ShowBuildSummaryAndLog BuildFailureReason FilePath

