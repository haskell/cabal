{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

-- | An experimental new UI for cabal for working with multiple packages
-----------------------------------------------------------------------------
module Distribution.Client.MultiPkg (
    -- * High level things
    configure,
    build,
  ) where

import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanning
import           Distribution.Client.ProjectBuilding

import           Distribution.Client.Types hiding (BuildResult, BuildSuccess(..), BuildFailure(..), DocsResult(..), TestsResult(..))
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.BuildTarget hiding (BuildTargetProblem, reportBuildTargetProblems)
import           Distribution.Client.DistDirLayout
import           Distribution.Client.Config (defaultCabalDir)
import           Distribution.Client.Setup hiding (packageName, cabalVersion)
import qualified Distribution.Client.Utils.Json as J

import qualified Distribution.Client.ComponentDeps as ComponentDeps

import           Distribution.Package hiding (InstalledPackageId, installedPackageId)
import qualified Distribution.PackageDescription as PD
import           Distribution.PackageDescription (FlagAssignment)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Simple.BuildTarget as Cabal

import           Distribution.Simple.Utils hiding (matchFileGlob)
import           Distribution.Verbosity
import           Distribution.Text
import qualified Paths_cabal_install (version)

import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.ByteString.Builder as BB

import           Control.Exception (bracket)
import           Control.Monad
import           Data.Monoid
import           Data.List
import           Data.Either
import           System.IO (hClose, openBinaryFile, IOMode(WriteMode))


------------------------------------------------------------------------------
-- * Understanding this module
------------------------------------------------------------------------------

-- This module deals with building and incrementally rebuilding a collection
-- of packages. It is what backs the "cabal build" and "configure" commands,
-- as well as being a core part of "run", "test", "bench" and others. 
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
-- * The "what to do" phase, where we look at the all input configuration
--   (project files, .cabal files, command line etc) and produce a detailed
--   plan of what to do -- the 'ElaboratedInstallPlan'.
--
-- * The "do it" phase, where we take the 'ElaboratedInstallPlan' and we
-- re-execute it.
--
-- As far as possible, the "what to do" phase embodies all the policy, leaving
-- the "do it" phase policy free. The first phase contains more of the
-- complicated logic, but it is contained in code that is either pure or just
-- has read effects (except cache updates). Then the second phase does all the
-- actions to build packages, but as far as possible it just follows the
-- instructions and avoids any logic for deciding what to do (apart from
-- recompilation avoidance in executing the plan).
--
-- This division helps us keep the code under control, making it easier to
-- understand, test and debug. So when you are extending this module, please
-- think about which parts of your change belong in which part. It is
-- perfectly ok to extend the description of what to do (i.e. the 
-- 'ElaboratedInstallPlan') if that helps keep the policy decisions in the
-- first phase. Also, the second phase does not have direct access to any of
-- the input configuration anyway; all the information has to flow via the
-- 'ElaboratedInstallPlan'.


------------------------------------------------------------------------------
-- * Top level commands: build and configure
------------------------------------------------------------------------------

build :: Verbosity
      -> GlobalFlags
      -> ConfigFlags
      -> ConfigExFlags
      -> InstallFlags
      -> Cabal.HaddockFlags
      -> [String]
      -> IO ()
build verbosity
      globalFlags
      configFlags configExFlags
      installFlags haddockFlags
      targetStrings = do

    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- findProjectRoot
    let distDirLayout = defaultDistDirLayout projectRootDir

    let cliConfig = commandLineFlagsToProjectConfig
                      globalFlags configFlags configExFlags
                      installFlags haddockFlags

    userTargets <- readUserBuildTargets targetStrings

    -- The (re)build is split into two major parts:
    --  * decide what to do
    --  * do it

    -- Phase 1: decide what to do.
    --
    -- 1.a) Take the project configuration and make a plan for how to build
    -- everything in the project. This is independent of any specific targets
    -- the user has asked for.
    --
    (elaboratedInstallPlan, sharedPackageConfig, projectConfig) <-
      rebuildInstallPlan verbosity
                         projectRootDir distDirLayout cabalDirLayout
                         cliConfig

    --TODO [code cleanup] move this inside the caching performed by
    -- rebuildInstallPlan so that we do not need to write it out every time
    writeFileBB (distProjectCacheFile distDirLayout "plan.json") $
               encodePlanToJson sharedPackageConfig elaboratedInstallPlan

    let buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          (projectConfigShared    projectConfig)
                          (projectConfigBuildOnly projectConfig)
                          (projectConfigBuildOnly cliConfig)

    -- The plan for what to do is represented by an 'ElaboratedInstallPlan'

    -- 1.b) Now given the specific targets the user has asked for, decide
    -- which bits of the plan we will want to execute.
    --
    elaboratedInstallPlan' <-
      selectTargets elaboratedInstallPlan
                    userTargets

    -- 1.c) Check if any packages don't need rebuilding, and improve the plan.
    -- This also gives us more accurate reasons for the --dry-run output.
    --
    (elaboratedInstallPlan'', pkgsBuildStatus) <-
      rebuildTargetsDryRun distDirLayout
                           elaboratedInstallPlan'

    -- Tell the user what we're going to do
    checkPrintPlan verbosity
                   elaboratedInstallPlan''
                   pkgsBuildStatus
                   buildSettings

    -- Phase 2: now do it.
    --
    -- Execute all or parts of the description of what to do to build or
    -- rebuild the various packages needed.
    --
    unless (buildSettingDryRun buildSettings) $ do
      _ <- rebuildTargets verbosity
                          distDirLayout
                          elaboratedInstallPlan''
                          sharedPackageConfig
                          pkgsBuildStatus
                          buildSettings
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

-- To a first approximation, configure just runs the first phase of 'build'
-- where we bring the install plan up to date (thus checking that it's
-- possible).
--
-- The only difference is that 'configure' also allows the user to specify
-- some extra local temporary config flags.
--

configure :: Verbosity
          -> GlobalFlags
          -> ConfigFlags
          -> ConfigExFlags
          -> InstallFlags
          -> Cabal.HaddockFlags
          -> [String]
          -> IO ()
configure verbosity
          globalFlags
          configFlags configExFlags
          installFlags haddockFlags
          _extraArgs = do
    --TODO: deal with _extraArgs, since flags with wrong syntax end up there

    -- In this prototype version we don't yet support extra local temporary
    -- config, so it's really just the first phase of build.

    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- findProjectRoot
    let distDirLayout = defaultDistDirLayout projectRootDir

    let cliConfig = commandLineFlagsToProjectConfig
                      globalFlags configFlags configExFlags
                      installFlags haddockFlags
    writeProjectLocalExtraConfig projectRootDir cliConfig

    (_elaboratedInstallPlan, _sharedPackageConfig, _projectConfig) <-
      rebuildInstallPlan verbosity
                         projectRootDir distDirLayout cabalDirLayout mempty

    --TODO: print what we would build.
    -- Hmm, but we don't have any targets. Could pick implicit target like "."
    -- Or should we say what's in the project (+deps) as a whole?

    return ()


------------------------------------------------------------------------------
-- * Taking user targets into account, selecting what to build
------------------------------------------------------------------------------


selectTargets :: ElaboratedInstallPlan
              -> [UserBuildTarget]
              -> IO ElaboratedInstallPlan
selectTargets installPlan userBuildTargets = do

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
                   $ resolveAndCheckTargets installPlan buildTargets

    -- Finally, prune the install plan to cover just those target packages
    -- and their deps.
    --
    return (pruneInstallPlanToTargets buildTargets' installPlan)
  where
    localPackages =
      [ (pkgDescription pkg, pkgSourceLocation pkg)
      | InstallPlan.Configured pkg <- InstallPlan.toList installPlan ]
      --TODO: [code cleanup] is there a better way to identify local packages?




data BuildTargetProblem
   = BuildTargetNotInProject PackageName
   | BuildTargetComponentNotProjectLocal (BuildTarget PackageName)
   | BuildTargetOptionalStanzaDisabled Bool
      -- ^ @True@: explicitly disabled by user
      -- @False@: disabled by solver

resolveAndCheckTargets :: ElaboratedInstallPlan
                       -> [BuildTarget PackageName]
                       -> Either [BuildTargetProblem]
                                 (Map InstalledPackageId [ComponentTarget])
resolveAndCheckTargets installPlan targets =
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
      = Right (ipkgid, BuildAllDefaultComponents)

    -- But if we ask to build an individual component, then that component
    -- had better be in a package that is local to the project.
    -- TODO: and if it's an optional stanza, then that stanza must be available
    checkTarget t@(BuildTargetComponent pn cn)
      | Just ipkgid <- Map.lookup pn projLocalPkgs
      = Right (ipkgid, BuildSpecificComponent (Cabal.BuildTargetComponent cn))

      | Map.member pn projAllPkgs
      = Left (BuildTargetComponentNotProjectLocal t)

    checkTarget t@(BuildTargetModule pn cn mn)
      | Just ipkgid <- Map.lookup pn projLocalPkgs
      = Right (ipkgid, BuildSpecificComponent (Cabal.BuildTargetModule cn mn))

      | Map.member pn projAllPkgs
      = Left (BuildTargetComponentNotProjectLocal t)

    checkTarget t@(BuildTargetFile pn cn fn)
      | Just ipkgid <- Map.lookup pn projLocalPkgs
      = Right (ipkgid, BuildSpecificComponent (Cabal.BuildTargetFile cn fn))

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


-------------------------------
-- Displaying plan info
--

checkPrintPlan :: Verbosity
               -> ElaboratedInstallPlan
               -> BuildStatusMap
               -> BuildTimeSettings
               -> IO ()
checkPrintPlan verbosity installPlan pkgsBuildStatus
               BuildTimeSettings{buildSettingDryRun} =
    printPlan verbosity buildSettingDryRun pkgsBuildStatus
              (linearizeInstallPlan installPlan)
    --TODO: [required eventually] see Install.hs for other things checkPrintPlan ought to do too


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

printPlan :: Verbosity
          -> Bool -- is dry run
          -> BuildStatusMap
          -> [ElaboratedReadyPackage]
          -> IO ()
printPlan verbosity _ _ [] = notice verbosity "Up to date"
printPlan verbosity dryRun pkgsBuildStatus pkgs
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
    wouldWill | dryRun    = "would"
              | otherwise = "will"

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
      = " (" ++ unwords
                  [ Cabal.showBuildTarget Cabal.QL1 (packageId pkg) t
                  | t <- pkgBuildTargets pkg ]
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
          (MonitoredValueChanged _) -> "configuration changed"
        BuildStatusConfigure reason -> showMonitorChangedReason reason
        BuildStatusDepsRebuilt    _ -> "dependency rebuilt"
        BuildStatusBuild
          (MonitoredValueChanged _) _ -> "additional components to build"
        BuildStatusBuild
          (MonitoredFileChanged  _) _ -> "package files changed"
        BuildStatusBuild reason _     -> showMonitorChangedReason reason
      BuildStatusUpToDate {} -> "up to date" -- doesn't happen

    showMonitorChangedReason (MonitoredFileChanged file) = "file " ++ file
    showMonitorChangedReason (MonitoredValueChanged _)   = "value changed"
    showMonitorChangedReason  MonitorFirstRun     = "first run"
    showMonitorChangedReason  MonitorCorruptCache = "cannot read state cache"

-- | For the benefit of debugging and some external tools, write out a
-- representation of the install plan.
--
encodePlanToJson :: ElaboratedSharedConfig -> ElaboratedInstallPlan -> BB.Builder
encodePlanToJson _sharedPackageConfig elaboratedInstallPlan =
    --TODO: [nice to have] include all of the sharedPackageConfig and all of
    --      the parts of the elaboratedInstallPlan
    J.encodeToBuilder $
      J.object [ "cabal-version"     J..= jdisplay Paths_cabal_install.version
               , "cabal-lib-version" J..= jdisplay cabalVersion
               , "install-plan"      J..= jsonIPlan
               ]
  where
    jsonIPlan = map toJ (InstallPlan.toList elaboratedInstallPlan)

    -- ipi :: InstalledPackageInfo
    toJ (InstallPlan.PreExisting ipi) =
      -- installed packages currently lack configuration information
      -- such as their flag settings or non-lib components.
      --
      -- TODO: how to find out whether package is "local"?
      J.object
        [ "type"       J..= J.String "pre-existing"
        , "id"         J..= jdisplay (installedUnitId ipi)
        , "components" J..= J.object
          [ "lib" J..= J.object [ "depends" J..= map jdisplay (installedDepends ipi) ] ]
        ]

    -- ecp :: ElaboratedConfiguredPackage
    toJ (InstallPlan.Configured ecp) =
      J.object
        [ "type"       J..= J.String "configured"
        , "id"         J..= (jdisplay . installedUnitId) ecp
        , "components" J..= components
        , "flags"      J..= J.object [ fn J..= v
                                     | (PD.FlagName fn,v) <- pkgFlagAssignment ecp ]
        ]
      where
        components = J.object
          [ comp2str c J..= J.object
            [ "depends" J..= map (jdisplay . installedUnitId) v ]
          | (c,v) <- ComponentDeps.toList (pkgDependencies ecp) ]

    toJ _ = error "encodePlanToJson: only expecting PreExisting and Configured"

    -- TODO: maybe move this helper to "ComponentDeps" module?
    --       Or maybe define a 'Text' instance?
    comp2str c = case c of
        ComponentDeps.ComponentLib     -> "lib"
        ComponentDeps.ComponentExe s   -> "exe:"   <> s
        ComponentDeps.ComponentTest s  -> "test:"  <> s
        ComponentDeps.ComponentBench s -> "bench:" <> s
        ComponentDeps.ComponentSetup   -> "setup"

    jdisplay :: Text a => a -> J.Value
    jdisplay = J.String . display

-- | Write a 'BB.Builder' to a file
writeFileBB :: FilePath -> BB.Builder -> IO ()
writeFileBB f bb =
    bracket (openBinaryFile f WriteMode) hClose (`BB.hPutBuilder` bb)
