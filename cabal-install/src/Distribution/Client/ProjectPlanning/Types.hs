{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types used while planning how to build everything in a project.
--
-- Primarily this is the 'ElaboratedInstallPlan'.
module Distribution.Client.ProjectPlanning.Types
  ( SolverInstallPlan

    -- * Elaborated install plan types
  , ElaboratedInstallPlan
  , ElaboratedInstalledPackageInfo
  , normaliseConfiguredPackage
  , ElaboratedConfiguredPackage (..)
  , showElaboratedInstallPlan
  , elabDistDirParams
  , elabExeDependencyPaths
  , elabLibDependencies
  , elabOrderLibDependencies
  , elabExeDependencies
  , elabOrderExeDependencies
  , elabSetupDependencies
  , elabPkgConfigDependencies
  , elabInplaceDependencyBuildCacheFiles
  , elabRequiresRegistration
  , dataDirsEnvironmentForPlan
  , elabPlanPackageName
  , elabConfiguredName
  , elabComponentName
  , ElaboratedPackageOrComponent (..)
  , ElaboratedComponent (..)
  , ElaboratedPackage (..)
  , pkgOrderDependencies
  , pkgStagedOrderDependencies
  , ElaboratedPlanPackage
  , ElaboratedSharedConfig (..)
  , pkgConfigStageToolchain
  , pkgConfigToolchain
  , pkgConfigCompiler
  , pkgConfigPlatform
  , pkgConfigCompilerProgs
  , pkgConfigBuildToolchain
  , pkgConfigBuildCompiler
  , pkgConfigBuildPlatform
  , pkgConfigBuildProgs
  , setPkgConfigCompilerProgs
  , elabToolchain
  , elabCompiler
  , elabPlatform
  , elabProgramDb
  , ElaboratedReadyPackage
  , BuildStyle (..)
  , MemoryOrDisk (..)
  , isInplaceBuildStyle
  , CabalFileText
  , NotPerComponentReason (..)
  , NotPerComponentBuildType (..)
  , whyNotPerComponent

    -- * Build targets
  , ComponentTarget (..)
  , showComponentTarget
  , showTestComponentTarget
  , showBenchComponentTarget
  , SubComponentTarget (..)
  , isSubLibComponentTarget
  , isForeignLibComponentTarget
  , isExeComponentTarget
  , isTestComponentTarget
  , isBenchComponentTarget
  , componentOptionalStanza
  , componentTargetName

    -- * Setup script
  , SetupScriptStyle (..)
  , SetupCliVersion (..)
  , setupCliVersion
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.PackageHash
import Distribution.Client.TargetSelector
  ( SubComponentTarget (..)
  )

import Distribution.Client.DistDirLayout
import Distribution.Client.InstallPlan
  ( GenericInstallPlan
  , GenericPlanPackage (..)
  )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectPlanning.Stage
  ( HasStage (..)
  , WithStage (..)
  , withoutStage
  )
import Distribution.Client.SolverInstallPlan
  ( SolverInstallPlan
  )
import Distribution.Client.Toolchain
  ( Stage (..)
  , Toolchain (..)
  , Toolchains
  , getStage
  , overStage
  )
import Distribution.Client.Types

import Distribution.Backpack
import Distribution.Backpack.ModuleShape

import Distribution.Compat.Graph (IsNode (..))
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import qualified Distribution.PackageDescription as Cabal
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import qualified Distribution.Simple.BuildTarget as Cabal
import Distribution.Simple.Compiler
import Distribution.Simple.InstallDirs (PathTemplate)
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.LocalBuildInfo
  ( ComponentName (..)
  , LibraryName (..)
  )
import Distribution.Simple.Program
import Distribution.Simple.Setup
  ( DumpBuildInfo (..)
  , HaddockTarget
  , ReplOptions
  , TestShowDetails
  )
import Distribution.Simple.Utils (cabalVersion, ordNub)
import Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.OptionalStanza
import Distribution.System
import Distribution.Types.ComponentRequestedSpec
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PkgconfigVersion
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Version

import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Distribution.Verbosity
import System.FilePath ((</>))
import Text.PrettyPrint (hsep, parens, text)

-- | The combination of an elaborated install plan plus a
-- 'ElaboratedSharedConfig' contains all the details necessary to be able
-- to execute the plan without having to make further policy decisions.
--
-- It does not include dynamic elements such as resources (such as http
-- connections).
type ElaboratedInstallPlan =
  GenericInstallPlan
    ElaboratedInstalledPackageInfo
    ElaboratedConfiguredPackage

type ElaboratedPlanPackage =
  GenericPlanPackage
    ElaboratedInstalledPackageInfo
    ElaboratedConfiguredPackage

-- | A pre-existing installed package in the elaborated plan, tagged with the
-- build 'Stage' it belongs to. The stage is part of the plan's node key, so
-- that the host and build copies of the same installed package (which share a
-- 'UnitId') are kept as distinct nodes under cross-compilation.
type ElaboratedInstalledPackageInfo = WithStage InstalledPackageInfo

-- | User-friendly display string for an 'ElaboratedPlanPackage'.
elabPlanPackageName :: Verbosity -> ElaboratedPlanPackage -> String
elabPlanPackageName verbosity (PreExisting ipkg)
  | verbosityLevel verbosity <= Normal = prettyShow (packageName ipkg)
  | otherwise = prettyShow (installedUnitId ipkg)
elabPlanPackageName verbosity (Configured elab) =
  elabConfiguredName verbosity elab
elabPlanPackageName verbosity (Installed elab) =
  elabConfiguredName verbosity elab

showElaboratedInstallPlan :: ElaboratedInstallPlan -> String
showElaboratedInstallPlan = InstallPlan.showInstallPlan_gen showNode
  where
    showNode pkg =
      InstallPlan.ShowPlanNode
        { InstallPlan.showPlanHerald = herald
        , InstallPlan.showPlanNeighbours = deps
        }
      where
        herald =
          hsep
            [ text (InstallPlan.showPlanPackageTag pkg)
            , InstallPlan.foldPlanPackage (const mempty) in_mem pkg
            , pretty (packageId pkg)
            , parens (pretty (nodeKey pkg))
            ]

        in_mem elab = case elabBuildStyle elab of
          BuildInplaceOnly InMemory -> parens (text "In Memory")
          _ -> mempty

        deps = InstallPlan.foldPlanPackage installed_deps local_deps pkg

        installed_deps = map pretty . nodeNeighbors

        local_deps cfg = [(if internal then text "+" else mempty) <> pretty (confInstId uid) | (uid, internal) <- elabLibDependencies cfg]

-- TODO: [code cleanup] decide if we really need this, there's not much in it, and in principle
--      even platform and compiler could be different if we're building things
--      like a server + client with ghc + ghcjs
data ElaboratedSharedConfig = ElaboratedSharedConfig
  { pkgConfigToolchains :: Toolchains
  -- ^ The host and build 'Toolchain's (compiler, platform, program database).
  -- In a non-cross build both stages hold the same toolchain; under cross-
  -- compilation the build stage carries the build-machine toolchain used for
  -- build-tools and custom @Setup.hs@ scripts. The per-stage values are read
  -- with 'getStage'; the host-stage and build-stage components also have the
  -- named accessors below.
  --
  -- The compiler program database gathers all known programs configured once
  -- for the project: the compiler (e.g. ghc & ghc-pkg) plus associated tools
  -- (hsc2hs, haddock, hpc, runghc) and toolchain programs (ar, ld, strip).
  -- Once constructed, only the 'configuredPrograms' are used.
  , pkgConfigReplOptions :: ReplOptions
  }
  deriving (Show, Generic)

-- TODO: [code cleanup] no Eq instance

instance Binary ElaboratedSharedConfig
instance Structured ElaboratedSharedConfig

-- | The toolchain for a given build 'Stage'.
pkgConfigStageToolchain :: ElaboratedSharedConfig -> Stage -> Toolchain
pkgConfigStageToolchain = getStage . pkgConfigToolchains

-- | The /host/ toolchain (the machine the built artifacts will run on), the
-- stage every package not reached through a tool dependency belongs to.
pkgConfigToolchain :: ElaboratedSharedConfig -> Toolchain
pkgConfigToolchain = flip pkgConfigStageToolchain Host

-- | The host compiler; see 'pkgConfigToolchain'.
pkgConfigCompiler :: ElaboratedSharedConfig -> Compiler
pkgConfigCompiler = toolchainCompiler . pkgConfigToolchain

-- | The host platform; see 'pkgConfigToolchain'.
pkgConfigPlatform :: ElaboratedSharedConfig -> Platform
pkgConfigPlatform = toolchainPlatform . pkgConfigToolchain

-- | The host toolchain's program database; see 'pkgConfigToolchain'.
pkgConfigCompilerProgs :: ElaboratedSharedConfig -> ProgramDb
pkgConfigCompilerProgs = toolchainProgramDb . pkgConfigToolchain

-- | The /build/ toolchain (the machine running the build), used for
-- build-tools and custom @Setup.hs@ scripts. Equal to the host toolchain
-- unless cross-compiling.
pkgConfigBuildToolchain :: ElaboratedSharedConfig -> Toolchain
pkgConfigBuildToolchain = flip pkgConfigStageToolchain Build

-- | The build compiler; see 'pkgConfigBuildToolchain'.
pkgConfigBuildCompiler :: ElaboratedSharedConfig -> Compiler
pkgConfigBuildCompiler = toolchainCompiler . pkgConfigBuildToolchain

-- | The build platform; see 'pkgConfigBuildToolchain'.
pkgConfigBuildPlatform :: ElaboratedSharedConfig -> Platform
pkgConfigBuildPlatform = toolchainPlatform . pkgConfigBuildToolchain

-- | The build toolchain's program database; see 'pkgConfigBuildToolchain'.
pkgConfigBuildProgs :: ElaboratedSharedConfig -> ProgramDb
pkgConfigBuildProgs = toolchainProgramDb . pkgConfigBuildToolchain

-- | Replace the /host/ toolchain's program database (e.g. to register a
-- freshly-configured @haddock@). The build toolchain is left untouched.
setPkgConfigCompilerProgs :: ProgramDb -> ElaboratedSharedConfig -> ElaboratedSharedConfig
setPkgConfigCompilerProgs progs shared =
  shared
    { pkgConfigToolchains =
        overStage Host (\tc -> tc{toolchainProgramDb = progs}) (pkgConfigToolchains shared)
    }

-- | The 'Toolchain' that configures a given elaborated package: the toolchain
-- of the package's own build 'Stage'. This is the only correct way to obtain
-- the compiler, platform, or program database an 'ElaboratedConfiguredPackage'
-- is built against — reaching for the host toolchain directly (e.g.
-- 'pkgConfigCompiler') silently assumes the 'Host' stage, which is wrong for a
-- build-stage package under cross-compilation. In a non-cross build every
-- package is on the host stage, so this selects the host toolchain there.
elabToolchain :: ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> Toolchain
elabToolchain shared elab = pkgConfigStageToolchain shared (elabStage elab)

-- | The compiler a package is built with; see 'elabToolchain'.
elabCompiler :: ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> Compiler
elabCompiler shared = toolchainCompiler . elabToolchain shared

-- | The platform a package is built for; see 'elabToolchain'.
elabPlatform :: ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> Platform
elabPlatform shared = toolchainPlatform . elabToolchain shared

-- | The program database of the toolchain a package is built with; see
-- 'elabToolchain'.
elabProgramDb :: ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> ProgramDb
elabProgramDb shared = toolchainProgramDb . elabToolchain shared

data ElaboratedConfiguredPackage = ElaboratedConfiguredPackage
  { elabUnitId :: UnitId
  -- ^ The 'UnitId' which uniquely identifies this item in a build plan
  , elabComponentId :: ComponentId
  , elabInstantiatedWith :: Map ModuleName Module
  , elabLinkedInstantiatedWith :: Map ModuleName OpenModule
  , elabIsCanonical :: Bool
  -- ^ This is true if this is an indefinite package, or this is a
  -- package with no signatures.  (Notably, it's not true for instantiated
  -- packages.)  The motivation for this is if you ask to build
  -- @foo-indef@, this probably means that you want to typecheck
  -- it, NOT that you want to rebuild all of the various
  -- instantiations of it.
  , elabPkgSourceId :: PackageId
  -- ^ The 'PackageId' of the originating package
  , elabStage :: Stage
  -- ^ The build 'Stage' this package is elaborated for. Under
  -- cross-compilation a package may appear on both the host and build
  -- stages; the stage selects which toolchain (compiler, platform, program
  -- database) configures it. In a non-cross build every package is on the
  -- host stage.
  , elabModuleShape :: ModuleShape
  -- ^ Shape of the package/component, for Backpack.
  , elabFlagAssignment :: Cabal.FlagAssignment
  -- ^ A total flag assignment for the package.
  -- TODO: Actually this can be per-component if we drop
  -- all flags that don't affect a component.
  , elabFlagDefaults :: Cabal.FlagAssignment
  -- ^ The original default flag assignment, used only for reporting.
  , elabPkgDescription :: Cabal.PackageDescription
  , elabGPkgDescription :: Cabal.GenericPackageDescription
  -- ^ Original 'GenericPackageDescription' (just used to report errors/warnings)
  , elabPkgSourceLocation :: PackageLocation (Maybe FilePath)
  -- ^ Where the package comes from, e.g. tarball, local dir etc. This
  --   is not the same as where it may be unpacked to for the build.
  , elabPkgSourceHash :: Maybe PackageSourceHash
  -- ^ The hash of the source, e.g. the tarball. We don't have this for
  -- local source dir packages.
  , elabLocalToProject :: Bool
  -- ^ Is this package one of the ones specified by location in the
  -- project file? (As opposed to a dependency, or a named package pulled
  -- in)
  , elabBuildStyle :: BuildStyle
  -- ^ Are we going to build and install this package to the store, or are
  -- we going to build it and register it locally.
  , elabEnabledSpec :: ComponentRequestedSpec
  -- ^ Another way of phrasing 'pkgStanzasAvailable'.
  , elabStanzasAvailable :: OptionalStanzaSet
  -- ^ Which optional stanzas (ie testsuites, benchmarks) can be built.
  -- This means the solver produced a plan that has them available.
  -- This doesn't necessary mean we build them by default.
  , elabStanzasRequested :: OptionalStanzaMap (Maybe Bool)
  -- ^ Which optional stanzas the user explicitly asked to enable or
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
  , elabPackageDbs :: [Maybe PackageDBCWD]
  , elabSetupPackageDBStack :: PackageDBStackCWD
  , elabBuildPackageDBStack :: PackageDBStackCWD
  , elabRegisterPackageDBStack :: PackageDBStackCWD
  , elabInplaceSetupPackageDBStack :: PackageDBStackCWD
  , elabInplaceBuildPackageDBStack :: PackageDBStackCWD
  , elabInplaceRegisterPackageDBStack :: PackageDBStackCWD
  , elabPkgDescriptionOverride :: Maybe CabalFileText
  , -- TODO: make per-component variants of these flags
    elabBuildOptions :: LBC.BuildOptions
  , elabDumpBuildInfo :: DumpBuildInfo
  , elabProgramPaths :: Map String FilePath
  , elabProgramArgs :: Map String [String]
  , elabProgramPathExtra :: [FilePath]
  , elabConfiguredPrograms :: [ConfiguredProgram]
  , elabConfigureScriptArgs :: [String]
  , elabExtraLibDirs :: [FilePath]
  , elabExtraLibDirsStatic :: [FilePath]
  , elabExtraFrameworkDirs :: [FilePath]
  , elabExtraIncludeDirs :: [FilePath]
  , elabProgPrefix :: Maybe PathTemplate
  , elabProgSuffix :: Maybe PathTemplate
  , elabInstallDirs :: InstallDirs.InstallDirs FilePath
  , elabHaddockHoogle :: Bool
  , elabHaddockHtml :: Bool
  , elabHaddockHtmlLocation :: Maybe String
  , elabHaddockForeignLibs :: Bool
  , elabHaddockForHackage :: HaddockTarget
  , elabHaddockExecutables :: Bool
  , elabHaddockTestSuites :: Bool
  , elabHaddockBenchmarks :: Bool
  , elabHaddockInternal :: Bool
  , elabHaddockCss :: Maybe FilePath
  , elabHaddockLinkedSource :: Bool
  , elabHaddockQuickJump :: Bool
  , elabHaddockHscolourCss :: Maybe FilePath
  , elabHaddockContents :: Maybe PathTemplate
  , elabHaddockIndex :: Maybe PathTemplate
  , elabHaddockBaseUrl :: Maybe String
  , elabHaddockResourcesDir :: Maybe String
  , elabHaddockOutputDir :: Maybe FilePath
  , elabHaddockUseUnicode :: Bool
  , elabTestMachineLog :: Maybe PathTemplate
  , elabTestHumanLog :: Maybe PathTemplate
  , elabTestShowDetails :: Maybe TestShowDetails
  , elabTestKeepTix :: Bool
  , elabTestWrapper :: Maybe FilePath
  , elabTestFailWhenNoTestSuites :: Bool
  , elabTestTestOptions :: [PathTemplate]
  , elabBenchmarkOptions :: [PathTemplate]
  , -- Setup.hs related things:

    elabSetupScriptStyle :: SetupScriptStyle
  -- ^ One of four modes for how we build and interact with the Setup.hs
  -- script, based on whether it's a build-type Custom or Hooks, with or
  -- without explicit deps, and the cabal spec version the .cabal file needs.
  , elabSetupScriptCliVersion :: SetupCliVersion
  -- ^ The Cabal library version used to provide the Setup CLI.
  , -- Build time related:
    elabConfigureTargets :: [ComponentTarget]
  , elabBuildTargets :: [ComponentTarget]
  , elabTestTargets :: [ComponentTarget]
  , elabBenchTargets :: [ComponentTarget]
  , elabReplTarget :: [ComponentTarget]
  , elabHaddockTargets :: [ComponentTarget]
  , elabBuildHaddocks :: Bool
  , -- pkgSourceDir ? -- currently passed in later because they can use temp locations
    -- pkgBuildDir  ? -- but could in principle still have it here, with optional instr to use temp loc

    elabPkgOrComp :: ElaboratedPackageOrComponent
  -- ^ Component/package specific information
  }
  deriving (Eq, Show, Generic)

normaliseConfiguredPackage
  :: ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> ElaboratedConfiguredPackage
normaliseConfiguredPackage shared pkg =
  pkg{elabProgramArgs = Map.mapMaybeWithKey lookupFilter (elabProgramArgs pkg)}
  where
    knownProgramDb = addKnownPrograms builtinPrograms (elabProgramDb shared pkg)

    pkgDesc :: PackageDescription
    pkgDesc = elabPkgDescription pkg

    removeEmpty :: [String] -> Maybe [String]
    removeEmpty [] = Nothing
    removeEmpty xs = Just xs

    lookupFilter :: String -> [String] -> Maybe [String]
    lookupFilter n args = removeEmpty $ case lookupKnownProgram n knownProgramDb of
      Just p -> programNormaliseArgs p (getVersion p) pkgDesc args
      Nothing -> args

    getVersion :: Program -> Maybe Version
    getVersion p = lookupProgram p knownProgramDb >>= programVersion

-- | The package/component contains/is a library and so must be registered
elabRequiresRegistration :: ElaboratedConfiguredPackage -> Bool
elabRequiresRegistration elab =
  case elabPkgOrComp elab of
    ElabComponent comp ->
      case compComponentName comp of
        Just cn -> is_lib cn && build_target
        _ -> False
    ElabPackage pkg ->
      -- Tricky! Not only do we have to test if the user selected
      -- a library as a build target, we also have to test if
      -- the library was TRANSITIVELY depended upon, since we will
      -- also require a register in this case.
      --
      -- NB: It would have been far nicer to just unconditionally
      -- register in all cases, but some Custom Setups will fall
      -- over if you try to do that, ESPECIALLY if there actually is
      -- a library but they hadn't built it.
      --
      -- However, as the case of `cpphs-1.20.8` has shown in
      -- #5379, in cases when a monolithic package gets
      -- installed due to its executable components
      -- (i.e. exe:cpphs) into the store we *have* to register
      -- if there's a buildable public library (i.e. lib:cpphs)
      -- that was built and installed into the same store folder
      -- as otherwise this will cause build failures once a
      -- target actually depends on lib:cpphs.
      build_target
        || ( elabBuildStyle elab == BuildAndInstall
              && Cabal.hasPublicLib (elabPkgDescription elab)
           )
        -- the next sub-condition below is currently redundant
        -- (see discussion in #5604 for more details), but it's
        -- being kept intentionally here as a safeguard because if
        -- internal libraries ever start working with
        -- non-per-component builds this condition won't be
        -- redundant anymore.
        || any (depends_on_lib pkg) (elabBuildTargets elab)
  where
    depends_on_lib pkg (ComponentTarget cn _) =
      not
        ( null
            ( CD.select
                (== CD.componentNameToComponent cn)
                (pkgDependsOnSelfLib pkg)
            )
        )
    build_target =
      if not (null (elabBuildTargets elab))
        then any is_lib_target (elabBuildTargets elab)
        else -- Empty build targets mean we build /everything/;
        -- that means we have to look more carefully to see
        -- if there is anything to register
          Cabal.hasLibs (elabPkgDescription elab)
    -- NB: this means we DO NOT reregister if you just built a
    -- single file
    is_lib_target (ComponentTarget cn WholeComponent) = is_lib cn
    is_lib_target _ = False
    is_lib (CLibName _) = True
    is_lib _ = False

-- | Construct the environment needed for the data files to work.
-- This consists of a separate @*_datadir@ variable for each
-- inplace package in the plan.
dataDirsEnvironmentForPlan
  :: DistDirLayout
  -> ElaboratedInstallPlan
  -> [(String, Maybe FilePath)]
dataDirsEnvironmentForPlan distDirLayout =
  mapMaybe
    ( InstallPlan.foldPlanPackage
        (const Nothing)
        (dataDirEnvVarForPackage distDirLayout)
    )
    . InstallPlan.toList

-- | Construct an environment variable that points
-- the package's datadir to its correct location.
-- This might be:
-- * 'Just' the package's source directory plus the data subdirectory
--   for inplace packages.
-- * 'Nothing' for packages installed in the store (the path was
--   already included in the package at install/build time).
dataDirEnvVarForPackage
  :: DistDirLayout
  -> ElaboratedConfiguredPackage
  -> Maybe (String, Maybe FilePath)
dataDirEnvVarForPackage distDirLayout pkg =
  case elabBuildStyle pkg of
    BuildAndInstall -> Nothing
    BuildInplaceOnly{} ->
      Just
        ( pkgPathEnvVar (elabPkgDescription pkg) "datadir"
        , Just dataDirPath
        )
  where
    srcPath (LocalUnpackedPackage path) = path
    srcPath (LocalTarballPackage _path) = unpackedPath
    srcPath (RemoteTarballPackage _uri _localTar) = unpackedPath
    srcPath (RepoTarballPackage _repo _packageId _localTar) = unpackedPath
    srcPath (RemoteSourceRepoPackage _sourceRepo (Just localCheckout)) = localCheckout
    -- TODO: see https://github.com/haskell/cabal/wiki/Potential-Refactors#unresolvedpkgloc
    srcPath (RemoteSourceRepoPackage _sourceRepo Nothing) =
      error
        "calling dataDirEnvVarForPackage on a not-downloaded repo is an error"
    unpackedPath =
      distUnpackedSrcDirectory distDirLayout $ elabPkgSourceId pkg
    rawDataDir = getSymbolicPath $ dataDir (elabPkgDescription pkg)
    pkgDir = srcPath (elabPkgSourceLocation pkg)
    dataDirPath
      | null rawDataDir =
          pkgDir
      | otherwise =
          pkgDir </> rawDataDir

-- NB: rawDataDir may be absolute, in which case
-- (</>) drops its first argument.

instance Package ElaboratedConfiguredPackage where
  packageId = elabPkgSourceId

instance HasConfiguredId ElaboratedConfiguredPackage where
  configuredId elab =
    ConfiguredId (packageId elab) (elabComponentName elab) (elabComponentId elab)

instance HasUnitId ElaboratedConfiguredPackage where
  installedUnitId = elabUnitId

instance IsNode ElaboratedConfiguredPackage where
  type Key ElaboratedConfiguredPackage = WithStage UnitId
  nodeKey elab = WithStage (elabStage elab) (elabUnitId elab)

  -- The guiding rule is: a neighbour's stage is whatever the solver assigned
  -- the dependency, and must be /read/ from the resolved plan node, never
  -- recomputed. Recomputation is what made build-tool/setup edges wrong before
  -- (they used @prevStage elabStage@, which invents the 'Build' stage even for
  -- a non-cross build that has no separate build stage). So build-tool and
  -- setup edges come from 'elabOrderExeDependencies' and
  -- 'elabSetupLibDependencies', both of which store the stage of the plan node
  -- the solver resolved the dependency to.
  --
  -- Library edges are the one exception, and use this package's own
  -- 'elabStage' rather than reading it off each resolved lib node. The more
  -- principled thing would be to read it, symmetrically with the exe/setup
  -- edges above, but we deliberately do not, for two reasons:
  --
  --   1. It would be redundant. A library edge is stage-/preserving/ by
  --      definition: the solver only changes stage across build-tool/setup
  --      boundaries (the single 'prevStage' in the solver's Dependency module),
  --      never across a library dependency, because a library links into a
  --      same-stage artifact. So a stage-@s@ node's library dependencies are
  --      always themselves at stage @s@ == 'elabStage', and reading the node
  --      would return exactly that.
  --   2. It is not even cleanly possible for a per-component build. There the
  --      library order dependencies ('compOrderLibDependencies') come from the
  --      Backpack mix-in linker output (@lc_includes@, abstract 'OpenUnitId's),
  --      not from the resolved 'SolverId' plan nodes, so there is no resolved
  --      node in hand to read a stage from without threading stage through the
  --      linker. Reading it only on the (easy) package path would leave the two
  --      paths asymmetric for no behavioural gain.
  --
  -- Setup dependencies are the deps of the @Setup.hs@ component, so they are
  -- excluded from 'libDeps' (which takes only the non-setup component deps)
  -- and added with their own (solver-assigned) stage instead. They are
  -- excluded by component, not by subtracting their 'UnitId's from the full
  -- order list: a library dependency and a setup dependency may share a
  -- 'UnitId' while sitting at different stages (e.g. @base@ when the build
  -- and host compilers agree on it), and subtracting by 'UnitId' would drop
  -- the library edge.
  nodeNeighbors elab =
    ordNub $
      map (WithStage (elabStage elab)) libDeps
        ++ elabOrderExeDependencies elab
        ++ map (fmap fromConfiguredId) (elabSetupLibDependencies elab)
    where
      libDeps = case elabPkgOrComp elab of
        ElabPackage pkg -> map (fromConfiguredId . fst) (CD.nonSetupDeps (pkgLibDependencies pkg))
        ElabComponent comp -> compOrderLibDependencies comp

instance HasStage ElaboratedConfiguredPackage where
  stageOf = elabStage

instance Binary ElaboratedConfiguredPackage
instance Structured ElaboratedConfiguredPackage

data ElaboratedPackageOrComponent
  = ElabPackage ElaboratedPackage
  | ElabComponent ElaboratedComponent
  deriving (Eq, Show, Generic)

instance Binary ElaboratedPackageOrComponent
instance Structured ElaboratedPackageOrComponent

elabComponentName :: ElaboratedConfiguredPackage -> Maybe ComponentName
elabComponentName elab =
  case elabPkgOrComp elab of
    ElabPackage _ -> Just $ CLibName LMainLibName -- there could be more, but default this
    ElabComponent comp -> compComponentName comp

-- | A user-friendly descriptor for an 'ElaboratedConfiguredPackage'.
elabConfiguredName :: Verbosity -> ElaboratedConfiguredPackage -> String
elabConfiguredName verbosity elab
  | verbosityLevel verbosity <= Normal =
      ( case elabPkgOrComp elab of
          ElabPackage _ -> ""
          ElabComponent comp ->
            case compComponentName comp of
              Nothing -> "setup from "
              Just (CLibName LMainLibName) -> ""
              Just cname -> prettyShow cname ++ " from "
      )
        ++ prettyShow (packageId elab)
  | otherwise =
      prettyShow (elabUnitId elab)

elabDistDirParams :: ElaboratedSharedConfig -> ElaboratedConfiguredPackage -> DistDirParams
elabDistDirParams shared elab =
  DistDirParams
    { distParamUnitId = installedUnitId elab
    , distParamComponentId = elabComponentId elab
    , distParamPackageId = elabPkgSourceId elab
    , distParamComponentName = case elabPkgOrComp elab of
        ElabComponent comp -> compComponentName comp
        ElabPackage _ -> Nothing
    , distParamCompilerId = compilerId (elabCompiler shared elab)
    , distParamPlatform = elabPlatform shared elab
    , distParamOptimization = LBC.withOptimization $ elabBuildOptions elab
    }

-- | The library "order dependencies" of a package: the 'UnitId's of the
-- libraries that must be built before it.  Used purely for build ordering.
elabOrderLibDependencies :: ElaboratedConfiguredPackage -> [UnitId]
elabOrderLibDependencies elab =
  case elabPkgOrComp elab of
    ElabPackage pkg ->
      map (newSimpleUnitId . confInstId) $
        ordNub $
          fold (map fst <$> pkgLibDependencies pkg)
    ElabComponent comp -> compOrderLibDependencies comp

-- | The library dependencies (i.e., the libraries we depend on, NOT
-- the dependencies of the library), NOT including setup dependencies.
-- These are passed to the @Setup@ script via @--dependency@ or @--promised-dependency@.
elabLibDependencies :: ElaboratedConfiguredPackage -> [(ConfiguredId, Bool)]
elabLibDependencies elab =
  case elabPkgOrComp elab of
    ElabPackage pkg -> ordNub (CD.nonSetupDeps (pkgLibDependencies pkg))
    ElabComponent comp -> compLibDependencies comp

-- | The executable "order dependencies" of a package.  (This coincides with
-- 'elabExeDependencies'.)  Each result carries the build 'Stage' the solver
-- assigned it.
elabOrderExeDependencies :: ElaboratedConfiguredPackage -> [WithStage UnitId]
elabOrderExeDependencies =
  map (fmap newSimpleUnitId) . elabExeDependencies

-- | The executable dependencies (i.e., the executables we depend on);
-- these are the executables we must add to the PATH before we invoke
-- the setup script.  Each is tagged with the build 'Stage' it was solved for.
elabExeDependencies :: ElaboratedConfiguredPackage -> [WithStage ComponentId]
elabExeDependencies elab = map (fmap confInstId) $
  case elabPkgOrComp elab of
    ElabPackage pkg -> CD.nonSetupDeps (pkgExeDependencies pkg)
    ElabComponent comp -> compExeDependencies comp

-- | This returns the paths of all the executables we depend on; we
-- must add these paths to PATH before invoking the setup script.
-- (This is usually what you want, not 'elabExeDependencies', if you
-- actually want to build something.)
elabExeDependencyPaths :: ElaboratedConfiguredPackage -> [FilePath]
elabExeDependencyPaths elab =
  case elabPkgOrComp elab of
    ElabPackage pkg -> map snd $ CD.nonSetupDeps (pkgExeDependencyPaths pkg)
    ElabComponent comp -> map snd (compExeDependencyPaths comp)

-- | The setup dependencies (the library dependencies of the setup executable;
-- note that it is not legal for setup scripts to have executable
-- dependencies at the moment.)
elabSetupDependencies :: ElaboratedConfiguredPackage -> [(ConfiguredId, Bool)]
elabSetupDependencies elab =
  case elabPkgOrComp elab of
    ElabPackage pkg -> CD.setupDeps (pkgLibDependencies pkg)
    -- TODO: Custom setups not supported for components yet.  When
    -- they are, need to do this differently
    ElabComponent _ -> []

-- | The library dependencies of the @Setup.hs@ script, each tagged with the
-- build 'Stage' the solver assigned it (see 'pkgSetupLibDependencies').  Unlike
-- 'elabSetupDependencies', this is used purely for ordering, so it carries the
-- stage but not the promised-dependency flag.
elabSetupLibDependencies :: ElaboratedConfiguredPackage -> [WithStage ConfiguredId]
elabSetupLibDependencies elab =
  case elabPkgOrComp elab of
    ElabPackage pkg -> pkgSetupLibDependencies pkg
    -- Custom setups are not supported for components.
    ElabComponent _ -> []

-- | Identify the plan node a 'ConfiguredId' dependency refers to.
fromConfiguredId :: ConfiguredId -> UnitId
fromConfiguredId = newSimpleUnitId . confInstId

elabPkgConfigDependencies :: ElaboratedConfiguredPackage -> [(PkgconfigName, Maybe PkgconfigVersion)]
elabPkgConfigDependencies ElaboratedConfiguredPackage{elabPkgOrComp = ElabPackage pkg} =
  pkgPkgConfigDependencies pkg
elabPkgConfigDependencies ElaboratedConfiguredPackage{elabPkgOrComp = ElabComponent comp} =
  compPkgConfigDependencies comp

-- | The cache files of all our inplace dependencies which,
-- when updated, require us to rebuild.  See #4202 for
-- more details.  Essentially, this is a list of filepaths
-- that, if our dependencies get rebuilt, will themselves
-- get updated.
--
-- Note: the hash of these cache files gets built into
-- the build cache ourselves, which means that we end
-- up tracking transitive dependencies!
--
-- Note: This tracks the "build" cache file, but not
-- "registration" or "config" cache files.  Why not?
-- Arguably we should...
--
-- Note: This is a bit of a hack, because it is not really
-- the hashes of the SOURCES of our (transitive) dependencies
-- that we should use to decide whether or not to rebuild,
-- but the output BUILD PRODUCTS.  The strategy we use
-- here will never work if we want to implement unchanging
-- rebuilds.
elabInplaceDependencyBuildCacheFiles
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> ElaboratedConfiguredPackage
  -> [FilePath]
elabInplaceDependencyBuildCacheFiles layout sconf plan root_elab =
  go =<< InstallPlan.directDeps plan (nodeKey root_elab)
  where
    go = InstallPlan.foldPlanPackage (const []) $ \elab -> do
      guard (isInplaceBuildStyle (elabBuildStyle elab))
      return $ distPackageCacheFile layout (elabDistDirParams sconf elab) "build"

-- | Some extra metadata associated with an
-- 'ElaboratedConfiguredPackage' which indicates that the "package"
-- in question is actually a single component to be built.  Arguably
-- it would be clearer if there were an ADT which branched into
-- package work items and component work items, but I've structured
-- it this way to minimize change to the existing code (which I
-- don't feel qualified to rewrite.)
data ElaboratedComponent = ElaboratedComponent
  { compSolverName :: CD.Component
  -- ^ The name of the component to be built according to the solver
  , compComponentName :: Maybe ComponentName
  -- ^ The name of the component to be built.  Nothing if
  -- it's a setup dep.
  , compLibDependencies :: [(ConfiguredId, Bool)]
  -- ^ The *external* library dependencies of this component.  We
  -- pass this to the configure script. The Bool indicates whether the
  -- dependency is a promised dependency (True) or not (False).
  , compLinkedLibDependencies :: [OpenUnitId]
  -- ^ In a component prior to instantiation, this list specifies
  -- the 'OpenUnitId's which, after instantiation, are the
  -- actual dependencies of this package.  Note that this does
  -- NOT include signature packages, which do not turn into real
  -- ordering dependencies when we instantiate.  This is intended to be
  -- a purely temporary field, to carry some information to the
  -- instantiation phase. It's more precise than
  -- 'compLibDependencies', and also stores information about internal
  -- dependencies.
  , compExeDependencies :: [WithStage ConfiguredId]
  -- ^ The executable dependencies of this component (including
  -- internal executables).  Each is tagged with the build 'Stage' it
  -- was solved for, so build-tool executables run during this
  -- component's build (the previous stage under cross-compilation) are
  -- kept distinct from same-named host artifacts.
  , compPkgConfigDependencies :: [(PkgconfigName, Maybe PkgconfigVersion)]
  -- ^ The @pkg-config@ dependencies of the component
  , compExeDependencyPaths :: [(WithStage ConfiguredId, FilePath)]
  -- ^ The paths all our executable dependencies will be installed
  -- to once they are installed.
  , compOrderLibDependencies :: [UnitId]
  -- ^ The UnitIds of the libraries (identifying elaborated packages/
  -- components) that must be built before this project.  This
  -- is used purely for ordering purposes.  It can contain both
  -- references to definite and indefinite packages; an indefinite
  -- UnitId indicates that we must typecheck that indefinite package
  -- before we can build this one.
  }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedComponent
instance Structured ElaboratedComponent

data ElaboratedPackage = ElaboratedPackage
  { pkgInstalledId :: InstalledPackageId
  , pkgLibDependencies :: ComponentDeps [(ConfiguredId, Bool)]
  -- ^ The exact dependencies (on other plan packages)
  -- The boolean value indicates whether the dependency is a promised dependency
  -- or not.
  , pkgDependsOnSelfLib :: ComponentDeps [()]
  -- ^ Components which depend (transitively) on an internally
  -- defined library.  These are used by 'elabRequiresRegistration',
  -- to determine if a user-requested build is going to need
  -- a library registration
  , pkgExeDependencies :: ComponentDeps [WithStage ConfiguredId]
  -- ^ Dependencies on executable packages, each tagged with the build
  -- 'Stage' it was solved for.
  , pkgExeDependencyPaths :: ComponentDeps [(WithStage ConfiguredId, FilePath)]
  -- ^ Paths where executable dependencies live.
  , pkgSetupLibDependencies :: [WithStage ConfiguredId]
  -- ^ The library dependencies of the @Setup.hs@ script, each tagged
  -- with the build 'Stage' the solver assigned it.  Setup scripts run
  -- during this package's build, so under cross-compilation these live
  -- on the previous ('Build') stage; we store the solver-assigned stage
  -- per dependency rather than recomputing it, because a non-cross build
  -- keeps them on the host stage (there is no separate build stage).
  , pkgPkgConfigDependencies :: [(PkgconfigName, Maybe PkgconfigVersion)]
  -- ^ Dependencies on @pkg-config@ packages.
  -- NB: this is NOT per-component (although it could be)
  -- because Cabal library does not track per-component
  -- pkg-config depends; it always does them all at once.
  , pkgStanzasEnabled :: OptionalStanzaSet
  -- ^ Which optional stanzas (ie testsuites, benchmarks) will actually
  -- be enabled during the package configure step.
  , pkgWhyNotPerComponent :: NE.NonEmpty NotPerComponentReason
  -- ^ Why is this not a per-component build?
  }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedPackage
instance Structured ElaboratedPackage

-- | Why did we fall-back to a per-package build, instead of using
-- a per-component build?
data NotPerComponentReason
  = -- | The build-type does not support per-component builds.
    CuzBuildType !NotPerComponentBuildType
  | -- | The Cabal spec version is too old for per-component builds.
    CuzCabalSpecVersion
  | -- | There are no buildable components, so we fall-back to a per-package
    -- build for error-reporting purposes.
    CuzNoBuildableComponents
  | -- | The user passed @--disable-per-component@.
    CuzDisablePerComponent
  deriving (Eq, Show, Generic)

data NotPerComponentBuildType
  = CuzConfigureBuildType
  | CuzCustomBuildType
  | CuzHooksBuildType
  deriving (Eq, Show, Generic)

instance Binary NotPerComponentBuildType
instance Structured NotPerComponentBuildType

instance Binary NotPerComponentReason
instance Structured NotPerComponentReason

-- | Display the reason we had to fall-back to a per-package build instead
-- of a per-component build.
whyNotPerComponent :: NotPerComponentReason -> String
whyNotPerComponent = \case
  CuzBuildType bt ->
    "build-type is " ++ case bt of
      CuzConfigureBuildType -> "Configure"
      CuzCustomBuildType -> "Custom"
      CuzHooksBuildType -> "Hooks"
  CuzCabalSpecVersion -> "cabal-version is less than 1.8"
  CuzNoBuildableComponents -> "there are no buildable components"
  CuzDisablePerComponent -> "you passed --disable-per-component"

-- | The per-component "order dependencies" (libraries and executables) of a
-- package, used purely for build ordering.  Unlike 'nodeNeighbors' this keeps
-- the 'ComponentDeps' structure rather than flattening it.
pkgOrderDependencies :: ElaboratedPackage -> ComponentDeps [UnitId]
pkgOrderDependencies pkg =
  fmap (map (newSimpleUnitId . confInstId)) (map fst <$> pkgLibDependencies pkg)
    <> fmap (map (newSimpleUnitId . confInstId . withoutStage)) (pkgExeDependencies pkg)

-- | 'pkgOrderDependencies' with every dependency tagged with the build 'Stage'
-- of the plan node it refers to, so that the result can be matched against
-- install plan keys.  Library edges are stage-preserving, so they take the
-- stage of the package itself; build-tool edges carry the stage the solver
-- assigned them.  (This is the same rule the 'IsNode' instance for
-- 'ElaboratedConfiguredPackage' follows, and for the same reasons.)
--
-- The @Setup.hs@ component's slot is /not/ meaningful here: setup library
-- dependencies sit at their own solver-assigned stage, recorded in
-- 'pkgSetupLibDependencies', which this does not consult.  Callers select
-- test and benchmark components only.
pkgStagedOrderDependencies :: Stage -> ElaboratedPackage -> ComponentDeps [WithStage UnitId]
pkgStagedOrderDependencies stage pkg =
  fmap (map (WithStage stage . newSimpleUnitId . confInstId)) (map fst <$> pkgLibDependencies pkg)
    <> fmap (map (fmap (newSimpleUnitId . confInstId))) (pkgExeDependencies pkg)

-- | This is used in the install plan to indicate how the package will be
-- built.
data BuildStyle
  = -- | The classic approach where the package is built, then the files
    -- installed into some location and the result registered in a package db.
    --
    -- If the package came from a tarball then it's built in a temp dir and
    -- the results discarded.
    BuildAndInstall
  | -- | For 'OnDisk': The package is built, but the files are not installed anywhere,
    -- rather the build dir is kept and the package is registered inplace.
    --
    -- Such packages can still subsequently be installed.
    --
    -- Typically 'BuildAndInstall' packages will only depend on other
    -- 'BuildAndInstall' style packages and not on 'BuildInplaceOnly' ones.
    --
    -- For 'InMemory':  Built in-memory only using GHC multi-repl, they are not built or installed
    -- anywhere on disk. BuildInMemory packages can't be depended on by BuildAndInstall nor BuildInplaceOnly packages
    -- (because they don't exist on disk) but can depend on other BuildStyles.
    --
    -- At the moment @'BuildInplaceOnly' 'InMemory'@ is only used by the 'repl' command.
    --
    -- We use single constructor 'BuildInplaceOnly' as for most cases
    -- inplace packages are handled similarly.
    BuildInplaceOnly MemoryOrDisk
  deriving (Eq, Ord, Show, Generic)

-- | How 'BuildInplaceOnly' component is built.
data MemoryOrDisk
  = OnDisk
  | InMemory
  deriving (Eq, Ord, Show, Generic)

-- Note: order of 'BuildStyle' and 'MemoryOrDisk' matters for 'Semigroup' / 'Monoid' instances

isInplaceBuildStyle :: BuildStyle -> Bool
isInplaceBuildStyle (BuildInplaceOnly{}) = True
isInplaceBuildStyle BuildAndInstall = False

instance Binary MemoryOrDisk
instance Structured MemoryOrDisk

instance Semigroup BuildStyle where
  -- 'BuildAndInstall' i.e. the smallest / first constructor is the unit.
  (<>) = max

instance Monoid BuildStyle where
  mempty = BuildAndInstall

instance Binary BuildStyle
instance Structured BuildStyle

type CabalFileText = LBS.ByteString

type ElaboratedReadyPackage = GenericReadyPackage ElaboratedConfiguredPackage

---------------------------
-- Build targets
--

-- | Specific targets within a package or component to act on e.g. to build,
-- haddock or open a repl.
data ComponentTarget = ComponentTarget ComponentName SubComponentTarget
  deriving (Eq, Ord, Show, Generic)

instance Binary ComponentTarget
instance Structured ComponentTarget

-- | Extract the component name from a 'ComponentTarget'.
componentTargetName :: ComponentTarget -> ComponentName
componentTargetName (ComponentTarget cname _) = cname

-- | Unambiguously render a 'ComponentTarget', e.g., to pass
-- to a Cabal Setup script.
showComponentTarget :: PackageId -> ComponentTarget -> String
showComponentTarget pkgid =
  Cabal.showBuildTarget pkgid . toBuildTarget
  where
    toBuildTarget :: ComponentTarget -> Cabal.BuildTarget
    toBuildTarget (ComponentTarget cname subtarget) =
      case subtarget of
        WholeComponent -> Cabal.BuildTargetComponent cname
        ModuleTarget mname -> Cabal.BuildTargetModule cname mname
        FileTarget fname -> Cabal.BuildTargetFile cname fname

showTestComponentTarget :: PackageId -> ComponentTarget -> Maybe String
showTestComponentTarget _ (ComponentTarget (CTestName n) _) = Just $ prettyShow n
showTestComponentTarget _ _ = Nothing

isTestComponentTarget :: ComponentTarget -> Bool
isTestComponentTarget (ComponentTarget (CTestName _) _) = True
isTestComponentTarget _ = False

showBenchComponentTarget :: PackageId -> ComponentTarget -> Maybe String
showBenchComponentTarget _ (ComponentTarget (CBenchName n) _) = Just $ prettyShow n
showBenchComponentTarget _ _ = Nothing

isBenchComponentTarget :: ComponentTarget -> Bool
isBenchComponentTarget (ComponentTarget (CBenchName _) _) = True
isBenchComponentTarget _ = False

isForeignLibComponentTarget :: ComponentTarget -> Bool
isForeignLibComponentTarget (ComponentTarget (CFLibName _) _) = True
isForeignLibComponentTarget _ = False

isExeComponentTarget :: ComponentTarget -> Bool
isExeComponentTarget (ComponentTarget (CExeName _) _) = True
isExeComponentTarget _ = False

isSubLibComponentTarget :: ComponentTarget -> Bool
isSubLibComponentTarget (ComponentTarget (CLibName (LSubLibName _)) _) = True
isSubLibComponentTarget _ = False

componentOptionalStanza :: CD.Component -> Maybe OptionalStanza
componentOptionalStanza (CD.ComponentTest _) = Just TestStanzas
componentOptionalStanza (CD.ComponentBench _) = Just BenchStanzas
componentOptionalStanza _ = Nothing

---------------------------
-- Setup.hs script policy
--

-- | There are four major cases for Setup.hs handling:
--
--  1. @build-type@ Custom or Hooks with a @custom-setup@ section
--  2. @build-type@ Custom without a @custom-setup@ section
--  3. @build-type@ neither Custom nor Hooks, with
--     @cabal-version >  $our-cabal-version@
--  4. @build-type@ neither Custom nor Hooks, with
--     @cabal-version <= $our-cabal-version@
--
-- It's also worth noting that packages specifying @cabal-version: >= 1.23@
-- or later that have @build-type@ Custom will always have a @custom-setup@
-- section. Therefore in case 2, the specified @cabal-version@ will always be
-- less than 1.23.
--
-- In cases 1 and 2 we obviously have to compile an external program: a
-- Setup.hs script for build-type Custom, and the hooks executable for
-- build-type Hooks (with a possible fallback to a Setup.hs).
-- In case 3 we also have to build an external Setup.hs script, because the
-- package needs a later Cabal lib version than we can support internally.
-- Only in case 4 can we use the internal library API alone.
data SetupScriptStyle
  = -- | @build-type: Custom@ (or @Hooks@) with explicit @setup-depends@
    SetupCustomExplicitDeps
  | -- | @build-type: Custom@ without an explicit @setup-depends@
    SetupCustomImplicitDeps
  | -- | Non-Custom/Hooks build-type, but we fall back to an external @Setup.hs@
    -- in order to satisfy Cabal version constraints.
    SetupNonCustomExternalLib
  | -- | Non-Custom/Hooks build type: Cabal provides the Setup.hs CLI internally.
    SetupNonCustomInternalLib
  deriving (Eq, Show, Generic)

instance Binary SetupScriptStyle
instance Structured SetupScriptStyle

-- | The version of the Cabal library used to provide the Setup CLI.
--
-- The version corresponds to the 'SetupScriptStyle' we use: for
-- 'SetupNonCustomInternalLib' we use the Cabal library that @cabal-install@ was
-- built against, and for every other 'SetupScripStyle' we pick the version
-- chosen by the solver to compile the Setup script.
data SetupCliVersion
  = -- | Use the Cabal library version that @cabal-install@ was linked against
    -- to provide the Setup CLI.
    InternalCabalLib -- NB: this very carefully __does not__ store a version number.
    --
    -- This is because of #11416: the install plan is cached across @cabal-install@
    -- invocations, so we should not pin a Cabal library version which would
    -- go stale when doing a minor @cabal-install@ upgrade.
  | -- | Use the Cabal library version picked by the solver to provide
    -- the Setup CLI.
    ExternalCabalLib !Version
  deriving (Eq, Show, Generic)

instance Binary SetupCliVersion
instance Structured SetupCliVersion

-- | The version of the Cabal library used to provide the Setup CLI.
setupCliVersion :: SetupCliVersion -> Version
setupCliVersion InternalCabalLib = cabalVersion
setupCliVersion (ExternalCabalLib version) = version
