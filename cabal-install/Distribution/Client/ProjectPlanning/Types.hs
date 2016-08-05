{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types used while planning how to build everything in a project.
--
-- Primarily this is the 'ElaboratedInstallPlan'.
--
module Distribution.Client.ProjectPlanning.Types (
    SolverInstallPlan,

    -- * Elaborated install plan types
    ElaboratedInstallPlan,
    ElaboratedConfiguredPackage(..),

    getElaboratedPackage,
    elabInstallDirs,
    elabDistDirParams,
    elabRequiresRegistration,
    elabBuildTargets,
    elabReplTarget,
    elabBuildHaddocks,

    ElaboratedComponent(..),
    ElaboratedPackage(..),
    ElaboratedPlanPackage,
    ElaboratedSharedConfig(..),
    ElaboratedReadyPackage,
    BuildStyle(..),
    CabalFileText,

    -- * Build targets
    PackageTarget(..),
    ComponentTarget(..),
    showComponentTarget,
    SubComponentTarget(..),

    -- * Setup script
    SetupScriptStyle(..),
  ) where

import           Distribution.Client.PackageHash

import           Distribution.Client.Types
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan, GenericPlanPackage )
import           Distribution.Client.SolverInstallPlan
                   ( SolverInstallPlan )
import           Distribution.Client.DistDirLayout

import           Distribution.Types.ComponentEnabledSpec
import           Distribution.Package
                   hiding (InstalledPackageId, installedPackageId)
import           Distribution.System
import qualified Distribution.PackageDescription as Cabal
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import           Distribution.Simple.Compiler
import qualified Distribution.Simple.BuildTarget as Cabal
import           Distribution.Simple.Program.Db
import           Distribution.ModuleName (ModuleName)
import           Distribution.Simple.LocalBuildInfo (ComponentName(..))
import qualified Distribution.Simple.InstallDirs as InstallDirs
import           Distribution.Simple.InstallDirs (PathTemplate)
import           Distribution.Version

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import           Distribution.Solver.Types.OptionalStanza
import           Distribution.Compat.Graph (IsNode(..))

import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.ByteString.Lazy as LBS
import           Distribution.Compat.Binary
import           GHC.Generics (Generic)



-- | The combination of an elaborated install plan plus a
-- 'ElaboratedSharedConfig' contains all the details necessary to be able
-- to execute the plan without having to make further policy decisions.
--
-- It does not include dynamic elements such as resources (such as http
-- connections).
--
type ElaboratedInstallPlan
   = GenericInstallPlan InstalledPackageInfo
                        ElaboratedConfiguredPackage

type ElaboratedPlanPackage
   = GenericPlanPackage InstalledPackageInfo
                        ElaboratedConfiguredPackage

--TODO: [code cleanup] decide if we really need this, there's not much in it, and in principle
--      even platform and compiler could be different if we're building things
--      like a server + client with ghc + ghcjs
data ElaboratedSharedConfig
   = ElaboratedSharedConfig {

       pkgConfigPlatform      :: Platform,
       pkgConfigCompiler      :: Compiler, --TODO: [code cleanup] replace with CompilerInfo
       -- | The programs that the compiler configured (e.g. for GHC, the progs
       -- ghc & ghc-pkg). Once constructed, only the 'configuredPrograms' are
       -- used.
       pkgConfigCompilerProgs :: ProgramDb
     }
  deriving (Show, Generic)
  --TODO: [code cleanup] no Eq instance

instance Binary ElaboratedSharedConfig

-- TODO: This is a misnomer, but I didn't want to rename things
-- willy-nilly yet
data ElaboratedConfiguredPackage
    = ElabPackage   ElaboratedPackage
    | ElabComponent ElaboratedComponent
  deriving (Eq, Show, Generic)

instance IsNode ElaboratedConfiguredPackage where
    type Key ElaboratedConfiguredPackage = UnitId
    nodeKey (ElabPackage pkg) = nodeKey pkg
    nodeKey (ElabComponent comp) = nodeKey comp
    nodeNeighbors (ElabPackage pkg) = nodeNeighbors pkg
    nodeNeighbors (ElabComponent comp) = nodeNeighbors comp

elabDistDirParams :: ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> DistDirParams
elabDistDirParams shared (ElabPackage pkg) = DistDirParams {
        distParamUnitId = installedUnitId pkg,
        distParamPackageId = pkgSourceId pkg,
        distParamComponentName = Nothing,
        distParamCompilerId = compilerId (pkgConfigCompiler shared),
        distParamPlatform = pkgConfigPlatform shared
    }
elabDistDirParams shared (ElabComponent comp) = DistDirParams {
        distParamUnitId = installedUnitId comp,
        distParamPackageId = packageId comp, -- NB: NOT the munged ID
        distParamComponentName = elabComponentName comp, -- TODO: Ick. Change type.
        distParamCompilerId = compilerId (pkgConfigCompiler shared),
        distParamPlatform = pkgConfigPlatform shared
    }

-- TODO: give each component a separate install dir prefix
elabInstallDirs :: ElaboratedConfiguredPackage -> InstallDirs.InstallDirs FilePath
elabInstallDirs = pkgInstallDirs . getElaboratedPackage

elabRequiresRegistration :: ElaboratedConfiguredPackage -> Bool
elabRequiresRegistration (ElabPackage pkg) = pkgRequiresRegistration pkg
elabRequiresRegistration (ElabComponent comp)
    = case elabComponent comp of
        CD.ComponentLib -> True
        CD.ComponentSubLib _ -> True
        _ -> False

elabBuildTargets :: ElaboratedConfiguredPackage -> [ComponentTarget]
elabBuildTargets (ElabPackage pkg) = pkgBuildTargets pkg
elabBuildTargets (ElabComponent comp)
    | Just cname <- elabComponentName comp
    = map (ComponentTarget cname) $ elabComponentBuildTargets comp
    | otherwise = []

elabReplTarget :: ElaboratedConfiguredPackage -> Maybe ComponentTarget
elabReplTarget (ElabPackage pkg) = pkgReplTarget pkg
elabReplTarget (ElabComponent comp)
    | Just cname <- elabComponentName comp
    = fmap (ComponentTarget cname) $ elabComponentReplTarget comp
    | otherwise = Nothing

elabBuildHaddocks :: ElaboratedConfiguredPackage -> Bool
elabBuildHaddocks (ElabPackage pkg) = pkgBuildHaddocks pkg
elabBuildHaddocks (ElabComponent comp) = elabComponentBuildHaddocks comp

getElaboratedPackage :: ElaboratedConfiguredPackage -> ElaboratedPackage
getElaboratedPackage (ElabPackage pkg) = pkg
getElaboratedPackage (ElabComponent comp) = elabComponentPackage comp

instance Binary ElaboratedConfiguredPackage

instance Package ElaboratedConfiguredPackage where
  packageId (ElabPackage pkg) = packageId pkg
  packageId (ElabComponent comp) = packageId comp

instance HasUnitId ElaboratedConfiguredPackage where
  installedUnitId (ElabPackage pkg) = installedUnitId pkg
  installedUnitId (ElabComponent comp) = installedUnitId comp

instance HasConfiguredId ElaboratedConfiguredPackage where
  configuredId (ElabPackage pkg)    = configuredId pkg
  configuredId (ElabComponent comp) = configuredId comp

-- | Some extra metadata associated with an
-- 'ElaboratedConfiguredPackage' which indicates that the "package"
-- in question is actually a single component to be built.  Arguably
-- it would be clearer if there were an ADT which branched into
-- package work items and component work items, but I've structured
-- it this way to minimize change to the existing code (which I
-- don't feel qualified to rewrite.)
data ElaboratedComponent
   = ElaboratedComponent {
    -- | The name of the component to be built
    elabComponent :: CD.Component,
    -- | The name of the component to be built.  Nothing if
    -- it's a setup dep.
    elabComponentName :: Maybe ComponentName,
    -- | The ID of the component to be built
    elabComponentId :: UnitId,
    -- | Dependencies of this component
    elabComponentDependencies :: [ConfiguredId],
    -- | The order-only dependencies of this component; e.g.,
    -- if you depend on an executable it goes here.
    elabComponentExeDependencies :: [ComponentId],
    -- | The 'ElaboratedPackage' this component came from
    elabComponentPackage :: ElaboratedPackage,
    -- | What in this component should we build (TRANSIENT, see 'pkgBuildTargets')
    elabComponentBuildTargets :: [SubComponentTarget],
    -- | Should we REPL this component (TRANSIENT, see 'pkgReplTarget')
    elabComponentReplTarget :: Maybe SubComponentTarget,
    -- | Should we Haddock this component (TRANSIENT, see 'pkgBuildHaddocks')
    elabComponentBuildHaddocks :: Bool
    -- NB: Careful, if you add elabComponentInstallDirs, need
    -- to adjust 'packageHashInputs'!!!
   }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedComponent

instance Package ElaboratedComponent where
    -- NB: DON'T return the munged ID by default.
    -- The 'Package' type class is about the source package
    -- name that the component belongs to; 'projAllPkgs'
    -- in "Distribution.Client.ProjectOrchestration" depends
    -- on this.
    packageId = packageId . elabComponentPackage

instance HasConfiguredId ElaboratedComponent where
    configuredId comp = ConfiguredId (packageId comp) (unitIdComponentId (elabComponentId comp))

instance HasUnitId ElaboratedComponent where
    installedUnitId = elabComponentId

instance IsNode ElaboratedComponent where
    type Key ElaboratedComponent = UnitId
    nodeKey = elabComponentId
    nodeNeighbors comp =
           -- TODO: Change this with Backpack!
           map (SimpleUnitId . confInstId) (elabComponentDependencies comp)
        ++ map SimpleUnitId (elabComponentExeDependencies comp)

data ElaboratedPackage
   = ElaboratedPackage {

       pkgInstalledId :: InstalledPackageId,
       pkgSourceId    :: PackageId,

       pkgDescription :: Cabal.PackageDescription,

       pkgInternalPackages :: Map PackageName ComponentName,

       -- | A total flag assignment for the package
       pkgFlagAssignment   :: Cabal.FlagAssignment,

       -- | The original default flag assignment, used only for reporting.
       pkgFlagDefaults     :: Cabal.FlagAssignment,

       -- | The exact dependencies (on other plan packages)
       --
       pkgDependencies     :: ComponentDeps [ConfiguredId],

       -- | The executable dependencies, which we don't pass as @--dependency@ flags;
       -- these just need to be added to the path.
       pkgExeDependencies :: ComponentDeps [ComponentId],

       -- | Another way of phrasing 'pkgStanzasAvailable'.
       pkgEnabled          :: ComponentEnabledSpec,

       -- | Which optional stanzas (ie testsuites, benchmarks) can be built.
       -- This means the solver produced a plan that has them available.
       -- This doesn't necessary mean we build them by default.
       pkgStanzasAvailable :: Set OptionalStanza,

       -- | Which optional stanzas the user explicitly asked to enable or
       -- to disable. This tells us which ones we build by default, and
       -- helps with error messages when the user asks to build something
       -- they explicitly disabled.
       --
       -- TODO: The 'Bool' here should be refined into an ADT with three
       -- cases: NotRequested, ExplicitlyRequested and
       -- ImplicitlyRequested.  A stanza is explicitly requested if
       -- the user asked, for this *specific* package, that the stanza
       -- be enabled; it's implicitly requested if the user asked for
       -- all global packages to have this stanza enabled.  The
       -- difference between an explicit and implicit request is
       -- error reporting behavior: if a user asks for tests to be
       -- enabled for a specific package that doesn't have any tests,
       -- we should warn them about it, but we shouldn't complain
       -- that a user enabled tests globally, and some local packages
       -- just happen not to have any tests.  (But perhaps we should
       -- warn if ALL local packages don't have any tests.)
       pkgStanzasRequested :: Map OptionalStanza Bool,

       -- | Which optional stanzas (ie testsuites, benchmarks) will actually
       -- be enabled during the package configure step.
       pkgStanzasEnabled :: Set OptionalStanza,

       -- | Where the package comes from, e.g. tarball, local dir etc. This
       --   is not the same as where it may be unpacked to for the build.
       pkgSourceLocation :: PackageLocation (Maybe FilePath),

       -- | The hash of the source, e.g. the tarball. We don't have this for
       -- local source dir packages.
       pkgSourceHash     :: Maybe PackageSourceHash,

       --pkgSourceDir ? -- currently passed in later because they can use temp locations
       --pkgBuildDir  ? -- but could in principle still have it here, with optional instr to use temp loc

       -- | Is this package one of the ones specified by location in the
       -- project file? (As opposed to a dependency, or a named package pulled
       -- in)
       pkgLocalToProject         :: Bool,

       -- | Are we going to build and install this package to the store, or are
       -- we going to build it and register it locally.
       pkgBuildStyle             :: BuildStyle,

       pkgSetupPackageDBStack    :: PackageDBStack,
       pkgBuildPackageDBStack    :: PackageDBStack,
       pkgRegisterPackageDBStack :: PackageDBStack,

       -- | The package contains a library and so must be registered
       pkgRequiresRegistration :: Bool,
       pkgDescriptionOverride  :: Maybe CabalFileText,

       pkgVanillaLib           :: Bool,
       pkgSharedLib            :: Bool,
       pkgDynExe               :: Bool,
       pkgGHCiLib              :: Bool,
       pkgProfLib              :: Bool,
       pkgProfExe              :: Bool,
       pkgProfLibDetail        :: ProfDetailLevel,
       pkgProfExeDetail        :: ProfDetailLevel,
       pkgCoverage             :: Bool,
       pkgOptimization         :: OptimisationLevel,
       pkgSplitObjs            :: Bool,
       pkgStripLibs            :: Bool,
       pkgStripExes            :: Bool,
       pkgDebugInfo            :: DebugInfoLevel,

       pkgProgramPaths          :: Map String FilePath,
       pkgProgramArgs           :: Map String [String],
       pkgProgramPathExtra      :: [FilePath],
       pkgConfigureScriptArgs   :: [String],
       pkgExtraLibDirs          :: [FilePath],
       pkgExtraFrameworkDirs    :: [FilePath],
       pkgExtraIncludeDirs      :: [FilePath],
       pkgProgPrefix            :: Maybe PathTemplate,
       pkgProgSuffix            :: Maybe PathTemplate,

       pkgInstallDirs           :: InstallDirs.InstallDirs FilePath,

       pkgHaddockHoogle         :: Bool,
       pkgHaddockHtml           :: Bool,
       pkgHaddockHtmlLocation   :: Maybe String,
       pkgHaddockExecutables    :: Bool,
       pkgHaddockTestSuites     :: Bool,
       pkgHaddockBenchmarks     :: Bool,
       pkgHaddockInternal       :: Bool,
       pkgHaddockCss            :: Maybe FilePath,
       pkgHaddockHscolour       :: Bool,
       pkgHaddockHscolourCss    :: Maybe FilePath,
       pkgHaddockContents       :: Maybe PathTemplate,

       -- Setup.hs related things:

       -- | One of four modes for how we build and interact with the Setup.hs
       -- script, based on whether it's a build-type Custom, with or without
       -- explicit deps and the cabal spec version the .cabal file needs.
       pkgSetupScriptStyle      :: SetupScriptStyle,

       -- | The version of the Cabal command line interface that we are using
       -- for this package. This is typically the version of the Cabal lib
       -- that the Setup.hs is built against.
       pkgSetupScriptCliVersion :: Version,

       -- Build time related:
       pkgBuildTargets          :: [ComponentTarget],
       pkgReplTarget            :: Maybe ComponentTarget,
       pkgBuildHaddocks         :: Bool
     }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedPackage

instance Package ElaboratedPackage where
  packageId = pkgSourceId

instance HasUnitId ElaboratedPackage where
  installedUnitId = SimpleUnitId . pkgInstalledId

instance HasConfiguredId ElaboratedPackage where
  configuredId pkg = ConfiguredId (pkgSourceId pkg) (pkgInstalledId pkg)

instance IsNode ElaboratedPackage where
  type Key ElaboratedPackage = UnitId
  nodeKey = installedUnitId
  nodeNeighbors pkg = map (SimpleUnitId . confInstId) (CD.flatDeps (pkgDependencies pkg))
                   ++ map SimpleUnitId (CD.flatDeps (pkgExeDependencies pkg))

-- | This is used in the install plan to indicate how the package will be
-- built.
--
data BuildStyle =
    -- | The classic approach where the package is built, then the files
    -- installed into some location and the result registered in a package db.
    --
    -- If the package came from a tarball then it's built in a temp dir and
    -- the results discarded.
    BuildAndInstall

    -- | The package is built, but the files are not installed anywhere,
    -- rather the build dir is kept and the package is registered inplace.
    --
    -- Such packages can still subsequently be installed.
    --
    -- Typically 'BuildAndInstall' packages will only depend on other
    -- 'BuildAndInstall' style packages and not on 'BuildInplaceOnly' ones.
    --
  | BuildInplaceOnly
  deriving (Eq, Show, Generic)

instance Binary BuildStyle

type CabalFileText = LBS.ByteString

type ElaboratedReadyPackage = GenericReadyPackage ElaboratedConfiguredPackage


---------------------------
-- Build targets
--

-- | The various targets within a package. This is more of a high level
-- specification than a elaborated prescription.
--
data PackageTarget =
     -- | Build the default components in this package. This usually means
     -- just the lib and exes, but it can also mean the testsuites and
     -- benchmarks if the user explicitly requested them.
     BuildDefaultComponents
     -- | Build a specific component in this package.
   | BuildSpecificComponent ComponentTarget
   | ReplDefaultComponent
   | ReplSpecificComponent  ComponentTarget
   | HaddockDefaultComponents
  deriving (Eq, Show, Generic)

data ComponentTarget = ComponentTarget ComponentName SubComponentTarget
  deriving (Eq, Show, Generic)

data SubComponentTarget = WholeComponent
                        | ModuleTarget ModuleName
                        | FileTarget   FilePath
  deriving (Eq, Show, Generic)

instance Binary PackageTarget
instance Binary ComponentTarget
instance Binary SubComponentTarget

-- | Unambiguously render a 'ComponentTarget', e.g., to pass
-- to a Cabal Setup script.
showComponentTarget :: PackageId -> ComponentTarget -> String
showComponentTarget pkgid =
    Cabal.showBuildTarget pkgid . toBuildTarget
  where
    toBuildTarget :: ComponentTarget -> Cabal.BuildTarget
    toBuildTarget (ComponentTarget cname subtarget) =
      case subtarget of
        WholeComponent     -> Cabal.BuildTargetComponent cname
        ModuleTarget mname -> Cabal.BuildTargetModule    cname mname
        FileTarget   fname -> Cabal.BuildTargetFile      cname fname



---------------------------
-- Setup.hs script policy
--

-- | There are four major cases for Setup.hs handling:
--
--  1. @build-type@ Custom with a @custom-setup@ section
--  2. @build-type@ Custom without a @custom-setup@ section
--  3. @build-type@ not Custom with @cabal-version >  $our-cabal-version@
--  4. @build-type@ not Custom with @cabal-version <= $our-cabal-version@
--
-- It's also worth noting that packages specifying @cabal-version: >= 1.23@
-- or later that have @build-type@ Custom will always have a @custom-setup@
-- section. Therefore in case 2, the specified @cabal-version@ will always be
-- less than 1.23.
--
-- In cases 1 and 2 we obviously have to build an external Setup.hs script,
-- while in case 4 we can use the internal library API. In case 3 we also have
-- to build an external Setup.hs script because the package needs a later
-- Cabal lib version than we can support internally.
--
data SetupScriptStyle = SetupCustomExplicitDeps
                      | SetupCustomImplicitDeps
                      | SetupNonCustomExternalLib
                      | SetupNonCustomInternalLib
  deriving (Eq, Show, Generic)

instance Binary SetupScriptStyle

