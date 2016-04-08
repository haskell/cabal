{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

-- | Types used while planning how to build everything in a project.
--
-- Primarily this is the 'ElaboratedInstallPlan'.
--
module Distribution.Client.ProjectPlanning.Types (
    SolverInstallPlan,

    -- * Elaborated install plan types
    ElaboratedInstallPlan,
    ElaboratedConfiguredPackage(..),
    ElaboratedPlanPackage,
    ElaboratedSharedConfig(..),
    ElaboratedReadyPackage,
    BuildStyle(..),
    CabalFileText,

    -- * Types used in executing an install plan
    --TODO: [code cleanup] these types should live with execution, not with
    --      plan definition. Need to better separate InstallPlan definition.
    GenericBuildResult(..),
    BuildResult,
    BuildSuccess(..),
    BuildFailure(..),
    DocsResult(..),
    TestsResult(..),

    -- * Build targets
    PackageTarget(..),
    ComponentTarget(..),
    SubComponentTarget(..),

    -- * Setup script
    SetupScriptStyle(..),
  ) where

import           Distribution.Client.PackageHash

import           Distribution.Client.Types
                   hiding ( BuildResult, BuildSuccess(..), BuildFailure(..)
                          , DocsResult(..), TestsResult(..) )
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan, InstallPlan, GenericPlanPackage )
import           Distribution.Client.ComponentDeps (ComponentDeps)

import           Distribution.Package
                   hiding (InstalledPackageId, installedPackageId)
import           Distribution.System
import qualified Distribution.PackageDescription as Cabal
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import           Distribution.Simple.Compiler
import           Distribution.Simple.Program.Db
import           Distribution.ModuleName (ModuleName)
import           Distribution.Simple.LocalBuildInfo (ComponentName(..))
import qualified Distribution.Simple.InstallDirs as InstallDirs
import           Distribution.Simple.InstallDirs (PathTemplate)
import           Distribution.Version

import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.ByteString.Lazy as LBS
import           Distribution.Compat.Binary
import           GHC.Generics (Generic)
import           Data.Typeable (Typeable)
import           Control.Exception



-- | The type of install plan produced by the solver and used as the starting
-- point for the 'ElaboratedInstallPlan'.
--
type SolverInstallPlan
   = InstallPlan --TODO: [code cleanup] redefine locally or move def to solver interface


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
                        BuildSuccess BuildFailure

type ElaboratedPlanPackage
   = GenericPlanPackage InstalledPackageInfo
                        ElaboratedConfiguredPackage
                        BuildSuccess BuildFailure

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

       pkgInstalledId :: InstalledPackageId,
       pkgSourceId    :: PackageId,

       -- | TODO: [code cleanup] we don't need this, just a few bits from it:
       --   build type, spec version
       pkgDescription :: Cabal.PackageDescription,

       -- | A total flag assignment for the package
       pkgFlagAssignment   :: Cabal.FlagAssignment,

       -- | The original default flag assignment, used only for reporting.
       pkgFlagDefaults     :: Cabal.FlagAssignment,

       -- | The exact dependencies (on other plan packages)
       --
       pkgDependencies     :: ComponentDeps [ConfiguredId],

       -- | Which optional stanzas (ie testsuites, benchmarks) can be built.
       -- This means the solver produced a plan that has them available.
       -- This doesn't necessary mean we build them by default.
       pkgStanzasAvailable :: Set OptionalStanza,

       -- | Which optional stanzas the user explicitly asked to enable or
       -- to disable. This tells us which ones we build by default, and
       -- helps with error messages when the user asks to build something
       -- they explicitly disabled.
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

instance Binary ElaboratedConfiguredPackage

instance Package ElaboratedConfiguredPackage where
  packageId = pkgSourceId

instance HasUnitId ElaboratedConfiguredPackage where
  installedUnitId = pkgInstalledId

instance PackageFixedDeps ElaboratedConfiguredPackage where
  depends = fmap (map installedPackageId) . pkgDependencies

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
                                                  InstalledPackageInfo

--TODO: [code cleanup] this duplicates the InstalledPackageInfo quite a bit in an install plan
-- because the same ipkg is used by many packages. So the binary file will be big.
-- Could we keep just (ipkgid, deps) instead of the whole InstalledPackageInfo?
-- or transform to a shared form when serialising / deserialising

data GenericBuildResult ipkg iresult ifailure
                  = BuildFailure ifailure
                  | BuildSuccess (Maybe ipkg) iresult
  deriving (Eq, Show, Generic)

instance (Binary ipkg, Binary iresult, Binary ifailure) =>
         Binary (GenericBuildResult ipkg iresult ifailure)

type BuildResult  = GenericBuildResult InstalledPackageInfo 
                                       BuildSuccess BuildFailure

data BuildSuccess = BuildOk DocsResult TestsResult
  deriving (Eq, Show, Generic)

data DocsResult  = DocsNotTried  | DocsFailed  | DocsOk
  deriving (Eq, Show, Generic)

data TestsResult = TestsNotTried | TestsOk
  deriving (Eq, Show, Generic)

data BuildFailure = PlanningFailed              --TODO: [required eventually] not yet used
                  | DependentFailed PackageId
                  | DownloadFailed  String      --TODO: [required eventually] not yet used
                  | UnpackFailed    String      --TODO: [required eventually] not yet used
                  | ConfigureFailed String
                  | BuildFailed     String
                  | TestsFailed     String      --TODO: [required eventually] not yet used
                  | InstallFailed   String
  deriving (Eq, Show, Typeable, Generic)

instance Exception BuildFailure

instance Binary BuildFailure
instance Binary BuildSuccess
instance Binary DocsResult
instance Binary TestsResult


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

