{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types used while planning how to build everything in a project.
--
-- Primarily this is the 'ElaboratedInstallPlan'.
module Distribution.Client.ProjectPlanning.Types
  ( SolverInstallPlan

    -- * Elaborated install plan types
  , ElaboratedInstallPlan
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
  , ElaboratedPlanPackage
  , ElaboratedSharedConfig (..)
  , ElaboratedReadyPackage
  , BuildStyle (..)
  , MemoryOrDisk (..)
  , isInplaceBuildStyle
  , CabalFileText

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

    -- * Setup script
  , SetupScriptStyle (..)
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
import Distribution.Client.SolverInstallPlan
  ( SolverInstallPlan
  )
import Distribution.Client.Types

import Distribution.Backpack
import Distribution.Backpack.ModuleShape

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
import Distribution.System
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PkgconfigVersion
import Distribution.Verbosity (normal)
import Distribution.Version

import Distribution.Compat.Graph (IsNode (..))
import Distribution.Simple.Utils (ordNub)
import Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.OptionalStanza

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Monoid as Mon
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
    InstalledPackageInfo
    ElaboratedConfiguredPackage

type ElaboratedPlanPackage =
  GenericPlanPackage
    InstalledPackageInfo
    ElaboratedConfiguredPackage

-- | User-friendly display string for an 'ElaboratedPlanPackage'.
elabPlanPackageName :: Verbosity -> ElaboratedPlanPackage -> String
elabPlanPackageName verbosity (PreExisting ipkg)
  | verbosity <= normal = prettyShow (packageName ipkg)
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
          ( hsep
              [ text (InstallPlan.showPlanPackageTag pkg)
              , InstallPlan.foldPlanPackage (const mempty) in_mem pkg
              , pretty (packageId pkg)
              , parens (pretty (nodeKey pkg))
              ]
          )

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
  { pkgConfigPlatform :: Platform
  , pkgConfigCompiler :: Compiler -- TODO: [code cleanup] replace with CompilerInfo
  , pkgConfigCompilerProgs :: ProgramDb
  -- ^ The programs that the compiler configured (e.g. for GHC, the progs
  -- ghc & ghc-pkg). Once constructed, only the 'configuredPrograms' are
  -- used.
  , pkgConfigReplOptions :: ReplOptions
  }
  deriving (Show, Generic, Typeable)

-- TODO: [code cleanup] no Eq instance

instance Binary ElaboratedSharedConfig
instance Structured ElaboratedSharedConfig

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
  , elabModuleShape :: ModuleShape
  -- ^ Shape of the package/component, for Backpack.
  , elabFlagAssignment :: Cabal.FlagAssignment
  -- ^ A total flag assignment for the package.
  -- TODO: Actually this can be per-component if we drop
  -- all flags that don't affect a component.
  , elabFlagDefaults :: Cabal.FlagAssignment
  -- ^ The original default flag assignment, used only for reporting.
  , elabPkgDescription :: Cabal.PackageDescription
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
  , elabPackageDbs :: [Maybe PackageDB]
  , elabSetupPackageDBStack :: PackageDBStack
  , elabBuildPackageDBStack :: PackageDBStack
  , elabRegisterPackageDBStack :: PackageDBStack
  , elabInplaceSetupPackageDBStack :: PackageDBStack
  , elabInplaceBuildPackageDBStack :: PackageDBStack
  , elabInplaceRegisterPackageDBStack :: PackageDBStack
  , elabPkgDescriptionOverride :: Maybe CabalFileText
  , -- TODO: make per-component variants of these flags
    elabVanillaLib :: Bool
  , elabSharedLib :: Bool
  , elabStaticLib :: Bool
  , elabDynExe :: Bool
  , elabFullyStaticExe :: Bool
  , elabGHCiLib :: Bool
  , elabProfLib :: Bool
  , elabProfExe :: Bool
  , elabProfLibDetail :: ProfDetailLevel
  , elabProfExeDetail :: ProfDetailLevel
  , elabCoverage :: Bool
  , elabOptimization :: OptimisationLevel
  , elabSplitObjs :: Bool
  , elabSplitSections :: Bool
  , elabStripLibs :: Bool
  , elabStripExes :: Bool
  , elabDebugInfo :: DebugInfoLevel
  , elabDumpBuildInfo :: DumpBuildInfo
  , elabProgramPaths :: Map String FilePath
  , elabProgramArgs :: Map String [String]
  , elabProgramPathExtra :: [FilePath]
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
  , elabHaddockLib :: Maybe String
  , elabHaddockOutputDir :: Maybe FilePath
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
  -- script, based on whether it's a build-type Custom, with or without
  -- explicit deps and the cabal spec version the .cabal file needs.
  , elabSetupScriptCliVersion :: Version
  -- ^ The version of the Cabal command line interface that we are using
  -- for this package. This is typically the version of the Cabal lib
  -- that the Setup.hs is built against.
  --
  -- TODO: We might want to turn this into a enum,
  -- yet different enum than 'CabalSpecVersion'.
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
  deriving (Eq, Show, Generic, Typeable)

normaliseConfiguredPackage
  :: ElaboratedSharedConfig
  -> ElaboratedConfiguredPackage
  -> ElaboratedConfiguredPackage
normaliseConfiguredPackage ElaboratedSharedConfig{pkgConfigCompilerProgs} pkg =
  pkg{elabProgramArgs = Map.mapMaybeWithKey lookupFilter (elabProgramArgs pkg)}
  where
    knownProgramDb = addKnownPrograms builtinPrograms pkgConfigCompilerProgs

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
  catMaybes
    . fmap
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
        , Just $
            srcPath (elabPkgSourceLocation pkg)
              </> dataDir (elabPkgDescription pkg)
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

instance Package ElaboratedConfiguredPackage where
  packageId = elabPkgSourceId

instance HasConfiguredId ElaboratedConfiguredPackage where
  configuredId elab =
    ConfiguredId (packageId elab) (elabComponentName elab) (elabComponentId elab)

instance HasUnitId ElaboratedConfiguredPackage where
  installedUnitId = elabUnitId

instance IsNode ElaboratedConfiguredPackage where
  type Key ElaboratedConfiguredPackage = UnitId
  nodeKey = elabUnitId
  nodeNeighbors = elabOrderDependencies

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
  | verbosity <= normal =
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
    , distParamCompilerId = compilerId (pkgConfigCompiler shared)
    , distParamPlatform = pkgConfigPlatform shared
    , distParamOptimization = elabOptimization elab
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
    ElabPackage pkg -> ordNub (CD.flatDeps (pkgOrderDependencies pkg))
    ElabComponent comp -> compOrderDependencies comp

-- | Like 'elabOrderDependencies', but only returns dependencies on
-- libraries.
elabOrderLibDependencies :: ElaboratedConfiguredPackage -> [UnitId]
elabOrderLibDependencies elab =
  case elabPkgOrComp elab of
    ElabPackage pkg ->
      map (newSimpleUnitId . confInstId) $
        ordNub $
          CD.flatDeps (map fst <$> pkgLibDependencies pkg)
    ElabComponent comp -> compOrderLibDependencies comp

-- | The library dependencies (i.e., the libraries we depend on, NOT
-- the dependencies of the library), NOT including setup dependencies.
-- These are passed to the @Setup@ script via @--dependency@ or @--promised-dependency@.
elabLibDependencies :: ElaboratedConfiguredPackage -> [(ConfiguredId, Bool)]
elabLibDependencies elab =
  case elabPkgOrComp elab of
    ElabPackage pkg -> ordNub (CD.nonSetupDeps (pkgLibDependencies pkg))
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
elabExeDependencies elab = map confInstId $
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
  , compExeDependencies :: [ConfiguredId]
  -- ^ The executable dependencies of this component (including
  -- internal executables).
  , compPkgConfigDependencies :: [(PkgconfigName, Maybe PkgconfigVersion)]
  -- ^ The @pkg-config@ dependencies of the component
  , compExeDependencyPaths :: [(ConfiguredId, FilePath)]
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

-- | See 'elabOrderDependencies'.
compOrderDependencies :: ElaboratedComponent -> [UnitId]
compOrderDependencies comp =
  compOrderLibDependencies comp
    ++ compOrderExeDependencies comp

-- | See 'elabOrderExeDependencies'.
compOrderExeDependencies :: ElaboratedComponent -> [UnitId]
compOrderExeDependencies = map (newSimpleUnitId . confInstId) . compExeDependencies

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
  , pkgExeDependencies :: ComponentDeps [ConfiguredId]
  -- ^ Dependencies on executable packages.
  , pkgExeDependencyPaths :: ComponentDeps [(ConfiguredId, FilePath)]
  -- ^ Paths where executable dependencies live.
  , pkgPkgConfigDependencies :: [(PkgconfigName, Maybe PkgconfigVersion)]
  -- ^ Dependencies on @pkg-config@ packages.
  -- NB: this is NOT per-component (although it could be)
  -- because Cabal library does not track per-component
  -- pkg-config depends; it always does them all at once.
  , pkgStanzasEnabled :: OptionalStanzaSet
  -- ^ Which optional stanzas (ie testsuites, benchmarks) will actually
  -- be enabled during the package configure step.
  }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedPackage
instance Structured ElaboratedPackage

-- | See 'elabOrderDependencies'.  This gives the unflattened version,
-- which can be useful in some circumstances.
pkgOrderDependencies :: ElaboratedPackage -> ComponentDeps [UnitId]
pkgOrderDependencies pkg =
  fmap (map (newSimpleUnitId . confInstId)) (map fst <$> pkgLibDependencies pkg)
    `Mon.mappend` fmap (map (newSimpleUnitId . confInstId)) (pkgExeDependencies pkg)

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
data SetupScriptStyle
  = SetupCustomExplicitDeps
  | SetupCustomImplicitDeps
  | SetupNonCustomExternalLib
  | SetupNonCustomInternalLib
  deriving (Eq, Show, Generic, Typeable)

instance Binary SetupScriptStyle
instance Structured SetupScriptStyle
