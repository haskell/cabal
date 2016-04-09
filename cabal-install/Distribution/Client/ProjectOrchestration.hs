{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

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
    selectTargets,
    printPlan,

    -- * Build phase: now do it.
    runProjectBuildPhase,
  ) where

import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanning
import           Distribution.Client.ProjectBuilding

import           Distribution.Client.Types
                   hiding ( BuildResult, BuildSuccess(..), BuildFailure(..)
                          , DocsResult(..), TestsResult(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.BuildTarget
                   ( UserBuildTarget, resolveUserBuildTargets
                   , BuildTarget(..), buildTargetPackage )
import           Distribution.Client.DistDirLayout
import           Distribution.Client.Config (defaultCabalDir)
import           Distribution.Client.Setup hiding (packageName)

import           Distribution.Package
                   hiding (InstalledPackageId, installedPackageId)
import qualified Distribution.PackageDescription as PD
import           Distribution.PackageDescription (FlagAssignment)
import qualified Distribution.InstalledPackageInfo as Installed
import           Distribution.Simple.Setup (HaddockFlags)

import           Distribution.Simple.Utils (die, notice)
import           Distribution.Verbosity
import           Distribution.Text

import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.List
import           Data.Either


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
       hookSelectPlanSubset :: ElaboratedInstallPlan
                            -> IO ElaboratedInstallPlan
     }

-- | This holds the context between the pre-build and build phases.
--
data ProjectBuildContext = ProjectBuildContext {
      distDirLayout    :: DistDirLayout,
      elaboratedPlan   :: ElaboratedInstallPlan,
      elaboratedShared :: ElaboratedSharedConfig,
      pkgsBuildStatus  :: BuildStatusMap,
      buildSettings    :: BuildTimeSettings
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

    projectRootDir <- findProjectRoot
    let distDirLayout = defaultDistDirLayout projectRootDir

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
    (elaboratedPlan, elaboratedShared, projectConfig) <-
      rebuildInstallPlan verbosity
                         projectRootDir distDirLayout cabalDirLayout
                         cliConfig

    let buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          (projectConfigShared    projectConfig)
                          (projectConfigBuildOnly projectConfig)
                          (projectConfigBuildOnly cliConfig)

    -- The plan for what to do is represented by an 'ElaboratedInstallPlan'

    -- Now given the specific targets the user has asked for, decide
    -- which bits of the plan we will want to execute.
    --
    elaboratedPlan' <- hookSelectPlanSubset elaboratedPlan

    -- Check if any packages don't need rebuilding, and improve the plan.
    -- This also gives us more accurate reasons for the --dry-run output.
    --
    (elaboratedPlan'', pkgsBuildStatus) <-
      rebuildTargetsDryRun distDirLayout
                           elaboratedPlan'

    return ProjectBuildContext {
      distDirLayout,
      elaboratedPlan = elaboratedPlan'',
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
                     -> IO ()
runProjectBuildPhase verbosity ProjectBuildContext {..} = do
    _ <- rebuildTargets verbosity
                        distDirLayout
                        elaboratedPlan
                        elaboratedShared
                        pkgsBuildStatus
                        buildSettings
    --TODO return the result plan and use it for other status reporting
    return ()

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
-- How to get the 'PackageTarget's from the 'UserBuildTarget' is customisable.
--
selectTargets :: PackageTarget
              -> (ComponentTarget -> PackageTarget)
              -> [UserBuildTarget]
              -> ElaboratedInstallPlan
              -> IO ElaboratedInstallPlan
selectTargets targetDefaultComponents targetSpecificComponent
              userBuildTargets installPlan = do

    -- Match the user targets against the available targets. If no targets are
    -- given this uses the package in the current directory, if any.
    --
    buildTargets <- resolveUserBuildTargets localPackages userBuildTargets
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
    buildTargets' <- either reportBuildTargetProblems return
                   $ resolveAndCheckTargets
                       targetDefaultComponents
                       targetSpecificComponent
                       installPlan
                       buildTargets

    -- Finally, prune the install plan to cover just those target packages
    -- and their deps.
    --
    return (pruneInstallPlanToTargets buildTargets' installPlan)
  where
    localPackages =
      [ (pkgDescription pkg, pkgSourceLocation pkg)
      | InstallPlan.Configured pkg <- InstallPlan.toList installPlan ]
      --TODO: [code cleanup] is there a better way to identify local packages?



resolveAndCheckTargets :: PackageTarget
                       -> (ComponentTarget -> PackageTarget)
                       -> ElaboratedInstallPlan
                       -> [BuildTarget PackageName]
                       -> Either [BuildTargetProblem]
                                 (Map InstalledPackageId [PackageTarget])
resolveAndCheckTargets targetDefaultComponents
                       targetSpecificComponent
                       installPlan targets =
    case partitionEithers (map checkTarget targets) of
      ([], targets') -> Right $ Map.fromListWith (++)
                                  [ (ipkgid, [t]) | (ipkgid, t) <- targets' ]
      (problems, _)  -> Left problems
  where
    -- TODO [required eventually] currently all build targets refer to packages
    -- inside the project. Ultimately this has to be generalised to allow
    -- referring to other packages and targets.

    -- We can ask to build any whole package, project-local or a dependency
    checkTarget (BuildTargetPackage pn)
      | Just ipkgid <- Map.lookup pn projAllPkgs
      = Right (ipkgid, targetDefaultComponents)

    -- But if we ask to build an individual component, then that component
    -- had better be in a package that is local to the project.
    -- TODO: and if it's an optional stanza, then that stanza must be available
    checkTarget t@(BuildTargetComponent pn cn)
      | Just ipkgid <- Map.lookup pn projLocalPkgs
      = Right (ipkgid, targetSpecificComponent
                         (ComponentTarget cn WholeComponent))

      | Map.member pn projAllPkgs
      = Left (BuildTargetComponentNotProjectLocal t)

    checkTarget t@(BuildTargetModule pn cn mn)
      | Just ipkgid <- Map.lookup pn projLocalPkgs
      = Right (ipkgid, BuildSpecificComponent (ComponentTarget cn (ModuleTarget mn)))

      | Map.member pn projAllPkgs
      = Left (BuildTargetComponentNotProjectLocal t)

    checkTarget t@(BuildTargetFile pn cn fn)
      | Just ipkgid <- Map.lookup pn projLocalPkgs
      = Right (ipkgid, BuildSpecificComponent (ComponentTarget cn (FileTarget fn)))

      | Map.member pn projAllPkgs
      = Left (BuildTargetComponentNotProjectLocal t)

    checkTarget t
      = Left (BuildTargetNotInProject (buildTargetPackage t))


    projAllPkgs, projLocalPkgs :: Map PackageName InstalledPackageId
    projAllPkgs =
      Map.fromList
        [ (packageName pkg, installedPackageId pkg)
        | pkg <- InstallPlan.toList installPlan ]

    projLocalPkgs =
      Map.fromList
        [ (packageName pkg, installedPackageId pkg)
        | InstallPlan.Configured pkg <- InstallPlan.toList installPlan
        , case pkgSourceLocation pkg of
            LocalUnpackedPackage _ -> True; _ -> False
          --TODO: [code cleanup] is there a better way to identify local packages?
        ]

    --TODO: [research required] what if the solution has multiple versions of this package?
    --      e.g. due to setup deps or due to multiple independent sets of
    --      packages being built (e.g. ghc + ghcjs in a project)

data BuildTargetProblem
   = BuildTargetNotInProject PackageName
   | BuildTargetComponentNotProjectLocal (BuildTarget PackageName)
   | BuildTargetOptionalStanzaDisabled Bool
      -- ^ @True@: explicitly disabled by user
      -- @False@: disabled by solver

reportBuildTargetProblems :: [BuildTargetProblem] -> IO a
reportBuildTargetProblems = die . unlines . map reportBuildTargetProblem

reportBuildTargetProblem :: BuildTargetProblem -> String
reportBuildTargetProblem (BuildTargetNotInProject pn) =
        "Cannot build the package " ++ display pn ++ ", it is not in this project."
     ++ "(either directly or indirectly). If you want to add it to the "
     ++ "project then edit the cabal.project file."

reportBuildTargetProblem (BuildTargetComponentNotProjectLocal t) =
        "The package " ++ display (buildTargetPackage t) ++ " is in the "
     ++ "project but it is not a locally unpacked package, so  "

reportBuildTargetProblem (BuildTargetOptionalStanzaDisabled _) = undefined


------------------------------------------------------------------------------
-- Displaying what we plan to do
--

-- | Print a user-oriented presentation of the install plan, indicating what
-- will be built.
--
printPlan :: Verbosity -> ProjectBuildContext -> IO ()
printPlan verbosity
          ProjectBuildContext {
            elaboratedPlan,
            pkgsBuildStatus,
            buildSettings = BuildTimeSettings{buildSettingDryRun}
          }

  | null pkgs
  = notice verbosity "Up to date"

  | verbosity >= verbose
  = notice verbosity $ unlines $
      ("In order, the following " ++ wouldWill ++ " be built:")
    : map showPkgAndReason pkgs

  | otherwise
  = notice verbosity $ unlines $
      ("In order, the following " ++ wouldWill
       ++ " be built (use -v for more details):")
    : map showPkg pkgs
  where
    pkgs = linearizeInstallPlan elaboratedPlan

    wouldWill | buildSettingDryRun = "would"
              | otherwise          = "will"

    showPkg pkg = display (packageId pkg)

    showPkgAndReason :: ElaboratedReadyPackage -> String
    showPkgAndReason (ReadyPackage pkg _) =
      display (packageId pkg) ++
      showTargets pkg ++
      showFlagAssignment (nonDefaultFlags pkg) ++
      showStanzas pkg ++
      let buildStatus = pkgsBuildStatus Map.! installedPackageId pkg in
      " (" ++ showBuildStatus buildStatus ++ ")"

    nonDefaultFlags :: ElaboratedConfiguredPackage -> FlagAssignment
    nonDefaultFlags pkg = pkgFlagAssignment pkg \\ pkgFlagDefaults pkg

    showStanzas pkg = concat
                    $ [ " *test"
                      | TestStanzas  `Set.member` pkgStanzasEnabled pkg ]
                   ++ [ " *bench"
                      | BenchStanzas `Set.member` pkgStanzasEnabled pkg ]

    showTargets pkg
      | null (pkgBuildTargets pkg) = ""
      | otherwise
      = " (" ++ unwords [ showComponentTarget pkg t | t <- pkgBuildTargets pkg ]
             ++ ")"

    -- TODO: [code cleanup] this should be a proper function in a proper place
    showFlagAssignment :: FlagAssignment -> String
    showFlagAssignment = concatMap ((' ' :) . showFlagValue)
    showFlagValue (f, True)   = '+' : showFlagName f
    showFlagValue (f, False)  = '-' : showFlagName f
    showFlagName (PD.FlagName f) = f

    showBuildStatus status = case status of
      BuildStatusPreExisting -> "already installed"
      BuildStatusDownload {} -> "requires download & build"
      BuildStatusUnpack   {} -> "requires build"
      BuildStatusRebuild _ rebuild -> case rebuild of
        BuildStatusConfigure
          (MonitoredValueChanged _)   -> "configuration changed"
        BuildStatusConfigure mreason  -> showMonitorChangedReason mreason
        BuildStatusBuild _ buildreason -> case buildreason of
          BuildReasonDepsRebuilt      -> "dependency rebuilt"
          BuildReasonFilesChanged
            (MonitoredFileChanged _)  -> "files changed"
          BuildReasonFilesChanged
            mreason                   -> showMonitorChangedReason mreason
          BuildReasonExtraTargets _   -> "additional components to build"
          BuildReasonEphemeralTargets -> "ephemeral targets"
      BuildStatusUpToDate {} -> "up to date" -- doesn't happen

    showMonitorChangedReason (MonitoredFileChanged file) = "file " ++ file
    showMonitorChangedReason (MonitoredValueChanged _)   = "value changed"
    showMonitorChangedReason  MonitorFirstRun     = "first run"
    showMonitorChangedReason  MonitorCorruptCache = "cannot read state cache"

linearizeInstallPlan :: ElaboratedInstallPlan -> [ElaboratedReadyPackage]
linearizeInstallPlan =
    unfoldr next
  where
    next plan = case InstallPlan.ready plan of
      []      -> Nothing
      (pkg:_) -> Just (pkg, plan')
        where
          ipkgid = installedPackageId pkg
          ipkg   = Installed.emptyInstalledPackageInfo {
                     Installed.sourcePackageId    = packageId pkg,
                     Installed.installedUnitId = ipkgid
                   }
          plan'  = InstallPlan.completed ipkgid (Just ipkg)
                     (BuildOk DocsNotTried TestsNotTried)
                     (InstallPlan.processing [pkg] plan)
    --TODO: [code cleanup] This is a bit of a hack, pretending that each package is installed
    -- could we use InstallPlan.topologicalOrder?

