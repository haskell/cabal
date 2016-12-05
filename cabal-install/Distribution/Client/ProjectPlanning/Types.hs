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
    elabOrderLibDependencies,
    elabExeDependencies,
    elabOrderExeDependencies,
    elabSetupDependencies,
    elabPkgConfigDependencies,

    elabPlanPackageName,
    elabConfiguredName,

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
    ComponentTarget(..),
    showComponentTarget,
    SubComponentTarget(..),

    -- * Setup script
    SetupScriptStyle(..),
  ) where

import           Distribution.Client.TargetSelector
                   ( SubComponentTarget(..) )
import           Distribution.Client.PackageHash

import           Distribution.Client.Types
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan, GenericPlanPackage(..) )
import           Distribution.Client.SolverInstallPlan
                   ( SolverInstallPlan )
import           Distribution.Client.DistDirLayout

import           Distribution.Backpack
import           Distribution.Backpack.ModuleShape

import           Distribution.Verbosity
import           Distribution.Text
import           Distribution.Types.ComponentRequestedSpec
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
import qualified Data.Monoid as Mon
import           Data.Typeable



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

-- | User-friendly display string for an 'ElaboratedPlanPackage'.
elabPlanPackageName :: Verbosity -> ElaboratedPlanPackage -> String
elabPlanPackageName verbosity (PreExisting ipkg)
    | verbosity <= normal = display (packageName ipkg)
    | otherwise           = display (installedUnitId ipkg)
elabPlanPackageName verbosity (Configured elab)
    = elabConfiguredName verbosity elab
elabPlanPackageName verbosity (Installed elab)
    = elabConfiguredName verbosity elab

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
  deriving (Show, Generic, Typeable)
  --TODO: [code cleanup] no Eq instance

instance Binary ElaboratedSharedConfig

data ElaboratedConfiguredPackage
   = ElaboratedConfiguredPackage {
       -- | The 'UnitId' which uniquely identifies this item in a build plan
       elabUnitId        :: UnitId,

       elabComponentId :: ComponentId,
       elabInstantiatedWith :: Map ModuleName Module,
       elabLinkedInstantiatedWith :: Map ModuleName OpenModule,

       -- | The 'PackageId' of the originating package
       elabPkgSourceId    :: PackageId,

       -- | Mapping from 'PackageName's to 'ComponentName', for every
       -- package that is overloaded with an internal component name
       elabInternalPackages :: Map PackageName ComponentName,

       -- | Shape of the package/component, for Backpack.
       elabModuleShape    :: ModuleShape,

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
       elabEnabledSpec      :: ComponentRequestedSpec,

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
       elabHaddockForeignLibs    :: Bool,
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
  deriving (Eq, Show, Generic, Typeable)

instance Package ElaboratedConfiguredPackage where
  packageId = elabPkgSourceId

instance HasConfiguredId ElaboratedConfiguredPackage where
  configuredId elab = ConfiguredId (packageId elab) (elabComponentId elab)

instance HasUnitId ElaboratedConfiguredPackage where
  installedUnitId = elabUnitId

instance IsNode ElaboratedConfiguredPackage where
    type Key ElaboratedConfiguredPackage = UnitId
    nodeKey = elabUnitId
    nodeNeighbors = elabOrderDependencies

instance Binary ElaboratedConfiguredPackage

data ElaboratedPackageOrComponent
    = ElabPackage   ElaboratedPackage
    | ElabComponent ElaboratedComponent
  deriving (Eq, Show, Generic)

instance Binary ElaboratedPackageOrComponent

-- | A user-friendly descriptor for an 'ElaboratedConfiguredPackage'.
elabConfiguredName :: Verbosity -> ElaboratedConfiguredPackage -> String
elabConfiguredName verbosity elab
    | verbosity <= normal
    = (case elabPkgOrComp elab of
        ElabPackage _ -> ""
        ElabComponent comp ->
            case compComponentName comp of
                Nothing -> "setup from "
                Just CLibName -> ""
                Just cname -> display cname ++ " from ")
      ++ display (packageId elab)
    | otherwise
    = display (elabUnitId elab)

elabDistDirParams :: ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> DistDirParams
elabDistDirParams shared elab = DistDirParams {
        distParamUnitId = installedUnitId elab,
        distParamComponentId = elabComponentId elab,
        distParamPackageId = elabPkgSourceId elab,
        distParamComponentName = case elabPkgOrComp elab of
            ElabComponent comp -> compComponentName comp
            ElabPackage _ -> Nothing,
        distParamCompilerId = compilerId (pkgConfigCompiler shared),
        distParamPlatform = pkgConfigPlatform shared,
        distParamOptimization = elabOptimization elab
    }

-- | The full set of dependencies which dictate what order we
-- need to build things in the install plan: "order dependencies"
-- balls everything together.  This is mostly only useful for
-- ordering; if you are, for example, trying to compute what
-- @--dependency@ flags to pass to a Setup script, you need to
-- use 'elabLibDependencies'.  This method is the same as
-- 'nodeNeighbors'.
--
-- NB: this method DOES include setup deps.
elabOrderDependencies :: ElaboratedConfiguredPackage -> [UnitId]
elabOrderDependencies elab =
    case elabPkgOrComp elab of
        -- Important not to have duplicates: otherwise InstallPlan gets
        -- confused.
        ElabPackage pkg    -> ordNub (CD.flatDeps (pkgOrderDependencies pkg))
        ElabComponent comp -> compOrderDependencies comp

-- | Like 'elabOrderDependencies', but only returns dependencies on
-- libraries.
elabOrderLibDependencies :: ElaboratedConfiguredPackage -> [UnitId]
elabOrderLibDependencies elab =
    case elabPkgOrComp elab of
        ElabPackage _      -> map (newSimpleUnitId . confInstId) (elabLibDependencies elab)
        ElabComponent comp -> compOrderLibDependencies comp

-- | The library dependencies (i.e., the libraries we depend on, NOT
-- the dependencies of the library), NOT including setup dependencies.
-- These are passed to the @Setup@ script via @--dependency@.
elabLibDependencies :: ElaboratedConfiguredPackage -> [ConfiguredId]
elabLibDependencies elab =
    case elabPkgOrComp elab of
        ElabPackage pkg    -> ordNub (CD.nonSetupDeps (pkgLibDependencies pkg))
        ElabComponent comp -> compLibDependencies comp

-- | Like 'elabOrderDependencies', but only returns dependencies on
-- executables.  (This coincides with 'elabExeDependencies'.)
elabOrderExeDependencies :: ElaboratedConfiguredPackage -> [UnitId]
elabOrderExeDependencies =
    map newSimpleUnitId . elabExeDependencies

-- | The executable dependencies (i.e., the executables we depend on);
-- these are the executables we must add to the PATH before we invoke
-- the setup script.
elabExeDependencies :: ElaboratedConfiguredPackage -> [ComponentId]
elabExeDependencies elab =
    case elabPkgOrComp elab of
        -- TODO: pkgExeDependencies being ConfiguredId is slightly awkward
        ElabPackage pkg    -> map confInstId (CD.nonSetupDeps (pkgExeDependencies pkg))
        ElabComponent comp -> compExeDependencies comp

-- | This returns the paths of all the executables we depend on; we
-- must add these paths to PATH before invoking the setup script.
-- (This is usually what you want, not 'elabExeDependencies', if you
-- actually want to build something.)
elabExeDependencyPaths :: ElaboratedConfiguredPackage -> [FilePath]
elabExeDependencyPaths elab =
    case elabPkgOrComp elab of
        ElabPackage pkg    -> CD.nonSetupDeps (pkgExeDependencyPaths pkg)
        ElabComponent comp -> compExeDependencyPaths comp

-- | The setup dependencies (the library dependencies of the setup executable;
-- note that it is not legal for setup scripts to have executable
-- dependencies at the moment.)
elabSetupDependencies :: ElaboratedConfiguredPackage -> [ConfiguredId]
elabSetupDependencies elab =
    case elabPkgOrComp elab of
        ElabPackage pkg    -> CD.setupDeps (pkgLibDependencies pkg)
        ElabComponent comp -> compSetupDependencies comp

elabPkgConfigDependencies :: ElaboratedConfiguredPackage -> [(PkgconfigName, Maybe Version)]
elabPkgConfigDependencies ElaboratedConfiguredPackage { elabPkgOrComp = ElabPackage pkg }
    = pkgPkgConfigDependencies pkg
elabPkgConfigDependencies ElaboratedConfiguredPackage { elabPkgOrComp = ElabComponent comp }
    = compPkgConfigDependencies comp

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
    -- | The *external* library dependencies of this component.  We
    -- pass this to the configure script.
    compLibDependencies :: [ConfiguredId],
    -- | The linked dependencies of the component which combined with the
    -- substitution in 'elabComponentId' specify the dependencies we
    -- care about from the perspective of ORDERING builds.  It's more
    -- precise than 'compLibDependencies', and also stores information
    -- about internal dependencies.
    compLinkedLibDependencies :: [OpenUnitId],
    -- | The executable dependencies of this component (including
    -- internal executables).
    compExeDependencies :: [ComponentId],
    -- | The @pkg-config@ dependencies of the component
    compPkgConfigDependencies :: [(PkgconfigName, Maybe Version)],
    -- | The paths all our executable dependencies will be installed
    -- to once they are installed.
    compExeDependencyPaths :: [FilePath],
    compNonSetupDependencies :: [UnitId],
    -- | The setup dependencies.  TODO: Remove this when setups
    -- are components of their own.
    compSetupDependencies :: [ConfiguredId]
   }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedComponent

-- | See 'elabOrderDependencies'.
compOrderDependencies :: ElaboratedComponent -> [UnitId]
compOrderDependencies comp =
    compOrderLibDependencies comp
 ++ compOrderExeDependencies comp

-- | See 'elabOrderExeDependencies'.
compOrderExeDependencies :: ElaboratedComponent -> [UnitId]
compOrderExeDependencies = map newSimpleUnitId . compExeDependencies

-- | See 'elabOrderLibDependencies'.
compOrderLibDependencies :: ElaboratedComponent -> [UnitId]
compOrderLibDependencies comp =
    compNonSetupDependencies comp
 ++ map (newSimpleUnitId . confInstId) (compSetupDependencies comp)

data ElaboratedPackage
   = ElaboratedPackage {
       pkgInstalledId :: InstalledPackageId,

       -- | The exact dependencies (on other plan packages)
       --
       pkgLibDependencies :: ComponentDeps [ConfiguredId],

       -- | Dependencies on executable packages.
       --
       pkgExeDependencies :: ComponentDeps [ConfiguredId],

       -- | Paths where executable dependencies live.
       --
       pkgExeDependencyPaths :: ComponentDeps [FilePath],

       -- | Dependencies on @pkg-config@ packages.
       -- NB: this is NOT per-component (although it could be)
       -- because Cabal library does not track per-component
       -- pkg-config depends; it always does them all at once.
       --
       pkgPkgConfigDependencies :: [(PkgconfigName, Maybe Version)],

       -- | Which optional stanzas (ie testsuites, benchmarks) will actually
       -- be enabled during the package configure step.
       pkgStanzasEnabled :: Set OptionalStanza
     }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedPackage

-- | See 'elabOrderDependencies'.  This gives the unflattened version,
-- which can be useful in some circumstances.
pkgOrderDependencies :: ElaboratedPackage -> ComponentDeps [UnitId]
pkgOrderDependencies pkg =
    fmap (map (newSimpleUnitId . confInstId)) (pkgLibDependencies pkg) `Mon.mappend`
    fmap (map (newSimpleUnitId . confInstId)) (pkgExeDependencies pkg)

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

-- | Specific targets within a package or component to act on e.g. to build,
-- haddock or open a repl.
--
data ComponentTarget = ComponentTarget ComponentName SubComponentTarget
  deriving (Eq, Ord, Show, Generic)

instance Binary ComponentTarget

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
  deriving (Eq, Show, Generic, Typeable)

instance Binary SetupScriptStyle

