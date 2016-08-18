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

    elabDistDirParams,
    elabExeDependencyPaths,
    elabLibDependencies,
    elabSetupDependencies,

    ElaboratedPackageOrComponent(..),
    ElaboratedComponent(..),
    ElaboratedPackage(..),
    pkgOrderDependencies,
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
import           Distribution.Simple.Utils (ordNub)

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

data ElaboratedConfiguredPackage
   = ElaboratedConfiguredPackage {
       -- | The 'UnitId' which uniquely identifies this item in a build plan
       elabUnitId        :: UnitId,

       -- | The 'PackageId' of the originating package
       elabPkgSourceId    :: PackageId,

       -- | Mapping from 'PackageName's to 'ComponentName', for every
       -- package that is overloaded with an internal component name
       elabInternalPackages :: Map PackageName ComponentName,

       -- | A total flag assignment for the package.
       -- TODO: Actually this can be per-component if we drop
       -- all flags that don't affect a component.
       elabFlagAssignment   :: Cabal.FlagAssignment,

       -- | The original default flag assignment, used only for reporting.
       elabFlagDefaults     :: Cabal.FlagAssignment,

       elabPkgDescription :: Cabal.PackageDescription,

       -- | Where the package comes from, e.g. tarball, local dir etc. This
       --   is not the same as where it may be unpacked to for the build.
       elabPkgSourceLocation :: PackageLocation (Maybe FilePath),

       -- | The hash of the source, e.g. the tarball. We don't have this for
       -- local source dir packages.
       elabPkgSourceHash     :: Maybe PackageSourceHash,

       -- | Is this package one of the ones specified by location in the
       -- project file? (As opposed to a dependency, or a named package pulled
       -- in)
       elabLocalToProject         :: Bool,

       -- | Are we going to build and install this package to the store, or are
       -- we going to build it and register it locally.
       elabBuildStyle             :: BuildStyle,

       -- | Another way of phrasing 'pkgStanzasAvailable'.
       elabEnabledSpec      :: ComponentEnabledSpec,

       -- | Which optional stanzas (ie testsuites, benchmarks) can be built.
       -- This means the solver produced a plan that has them available.
       -- This doesn't necessary mean we build them by default.
       elabStanzasAvailable :: Set OptionalStanza,

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
       elabStanzasRequested :: Map OptionalStanza Bool,

       elabSetupPackageDBStack    :: PackageDBStack,
       elabBuildPackageDBStack    :: PackageDBStack,
       elabRegisterPackageDBStack :: PackageDBStack,

       -- | The package/component contains/is a library and so must be registered
       elabRequiresRegistration :: Bool,

       elabPkgDescriptionOverride  :: Maybe CabalFileText,

       -- TODO: make per-component variants of these flags
       elabVanillaLib           :: Bool,
       elabSharedLib            :: Bool,
       elabDynExe               :: Bool,
       elabGHCiLib              :: Bool,
       elabProfLib              :: Bool,
       elabProfExe              :: Bool,
       elabProfLibDetail        :: ProfDetailLevel,
       elabProfExeDetail        :: ProfDetailLevel,
       elabCoverage             :: Bool,
       elabOptimization         :: OptimisationLevel,
       elabSplitObjs            :: Bool,
       elabStripLibs            :: Bool,
       elabStripExes            :: Bool,
       elabDebugInfo            :: DebugInfoLevel,

       elabProgramPaths          :: Map String FilePath,
       elabProgramArgs           :: Map String [String],
       elabProgramPathExtra      :: [FilePath],
       elabConfigureScriptArgs   :: [String],
       elabExtraLibDirs          :: [FilePath],
       elabExtraFrameworkDirs    :: [FilePath],
       elabExtraIncludeDirs      :: [FilePath],
       elabProgPrefix            :: Maybe PathTemplate,
       elabProgSuffix            :: Maybe PathTemplate,

       elabInstallDirs           :: InstallDirs.InstallDirs FilePath,

       elabHaddockHoogle         :: Bool,
       elabHaddockHtml           :: Bool,
       elabHaddockHtmlLocation   :: Maybe String,
       elabHaddockExecutables    :: Bool,
       elabHaddockTestSuites     :: Bool,
       elabHaddockBenchmarks     :: Bool,
       elabHaddockInternal       :: Bool,
       elabHaddockCss            :: Maybe FilePath,
       elabHaddockHscolour       :: Bool,
       elabHaddockHscolourCss    :: Maybe FilePath,
       elabHaddockContents       :: Maybe PathTemplate,

       -- Setup.hs related things:

       -- | One of four modes for how we build and interact with the Setup.hs
       -- script, based on whether it's a build-type Custom, with or without
       -- explicit deps and the cabal spec version the .cabal file needs.
       elabSetupScriptStyle      :: SetupScriptStyle,

       -- | The version of the Cabal command line interface that we are using
       -- for this package. This is typically the version of the Cabal lib
       -- that the Setup.hs is built against.
       elabSetupScriptCliVersion :: Version,

       -- Build time related:
       elabBuildTargets          :: [ComponentTarget],
       elabReplTarget            :: Maybe ComponentTarget,
       elabBuildHaddocks         :: Bool,

       --pkgSourceDir ? -- currently passed in later because they can use temp locations
       --pkgBuildDir  ? -- but could in principle still have it here, with optional instr to use temp loc

       -- | Component/package specific information
       elabPkgOrComp :: ElaboratedPackageOrComponent
   }
  deriving (Eq, Show, Generic)

instance Package ElaboratedConfiguredPackage where
  packageId = elabPkgSourceId

instance HasConfiguredId ElaboratedConfiguredPackage where
  configuredId elab = ConfiguredId (packageId elab) (unitIdComponentId (elabUnitId elab))

instance HasUnitId ElaboratedConfiguredPackage where
  installedUnitId = elabUnitId

instance IsNode ElaboratedConfiguredPackage where
    type Key ElaboratedConfiguredPackage = UnitId
    nodeKey = elabUnitId
    nodeNeighbors elab = case elabPkgOrComp elab of
        -- Important not to have duplicates: otherwise InstallPlan gets
        -- confused.  NB: this DOES include setup deps.
        ElabPackage pkg    -> ordNub (CD.flatDeps (pkgOrderDependencies pkg))
        ElabComponent comp -> compOrderDependencies comp

instance Binary ElaboratedConfiguredPackage

data ElaboratedPackageOrComponent
    = ElabPackage   ElaboratedPackage
    | ElabComponent ElaboratedComponent
  deriving (Eq, Show, Generic)

instance Binary ElaboratedPackageOrComponent

elabDistDirParams :: ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> DistDirParams
elabDistDirParams shared elab = DistDirParams {
        distParamUnitId = installedUnitId elab,
        distParamPackageId = elabPkgSourceId elab,
        distParamComponentName = case elabPkgOrComp elab of
            ElabComponent comp -> compComponentName comp
            ElabPackage _ -> Nothing,
        distParamCompilerId = compilerId (pkgConfigCompiler shared),
        distParamPlatform = pkgConfigPlatform shared
    }

-- | The library dependencies (i.e., the libraries we depend on, NOT
-- the dependencies of the library), NOT including setup dependencies.
elabLibDependencies :: ElaboratedConfiguredPackage -> [ConfiguredId]
elabLibDependencies ElaboratedConfiguredPackage { elabPkgOrComp = ElabPackage pkg }
    = ordNub (CD.nonSetupDeps (pkgLibDependencies pkg))
elabLibDependencies ElaboratedConfiguredPackage { elabPkgOrComp = ElabComponent comp }
    = compLibDependencies comp

elabExeDependencyPaths :: ElaboratedConfiguredPackage -> [FilePath]
elabExeDependencyPaths ElaboratedConfiguredPackage { elabPkgOrComp = ElabPackage _ }
    = [] -- TODO: not implemented
elabExeDependencyPaths ElaboratedConfiguredPackage { elabPkgOrComp = ElabComponent comp }
    = compExeDependencyPaths comp

elabSetupDependencies :: ElaboratedConfiguredPackage -> [ConfiguredId]
elabSetupDependencies ElaboratedConfiguredPackage { elabPkgOrComp = ElabPackage pkg }
    = CD.setupDeps (pkgLibDependencies pkg)
elabSetupDependencies ElaboratedConfiguredPackage { elabPkgOrComp = ElabComponent comp }
    = compSetupDependencies comp


-- | Some extra metadata associated with an
-- 'ElaboratedConfiguredPackage' which indicates that the "package"
-- in question is actually a single component to be built.  Arguably
-- it would be clearer if there were an ADT which branched into
-- package work items and component work items, but I've structured
-- it this way to minimize change to the existing code (which I
-- don't feel qualified to rewrite.)
data ElaboratedComponent
   = ElaboratedComponent {
    -- | The name of the component to be built according to the solver
    compSolverName :: CD.Component,
    -- | The name of the component to be built.  Nothing if
    -- it's a setup dep.
    compComponentName :: Maybe ComponentName,
    -- | The library dependencies of this component.
    compLibDependencies :: [ConfiguredId],
    -- | The executable dependencies of this component.
    compExeDependencies :: [ComponentId],
    -- | The paths all our executable dependencies will be installed
    -- to once they are installed.
    compExeDependencyPaths :: [FilePath],
    -- | The setup dependencies.  TODO: Remove this when setups
    -- are components of their own.
    compSetupDependencies :: [ConfiguredId]
   }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedComponent

compOrderDependencies :: ElaboratedComponent -> [UnitId]
compOrderDependencies comp =
       -- TODO: Change this with Backpack!
       map (SimpleUnitId . confInstId) (compLibDependencies comp)
    ++ map SimpleUnitId (compExeDependencies comp)
    ++ map (SimpleUnitId . confInstId) (compSetupDependencies comp)

data ElaboratedPackage
   = ElaboratedPackage {
       pkgInstalledId :: InstalledPackageId,

       -- | The exact dependencies (on other plan packages)
       --
       pkgLibDependencies :: ComponentDeps [ConfiguredId],

       -- | Which optional stanzas (ie testsuites, benchmarks) will actually
       -- be enabled during the package configure step.
       pkgStanzasEnabled :: Set OptionalStanza
     }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedPackage

pkgOrderDependencies :: ElaboratedPackage -> ComponentDeps [UnitId]
pkgOrderDependencies pkg =
    fmap (map (SimpleUnitId . confInstId)) (pkgLibDependencies pkg)

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

