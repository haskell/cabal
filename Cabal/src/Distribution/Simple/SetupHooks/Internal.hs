{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Distribution.Simple.SetupHooks.Internal
--
-- Internal implementation module.
-- Users of @build-type: Hooks@ should import "Distribution.Simple.SetupHooks"
-- instead.
module Distribution.Simple.SetupHooks.Internal
  ( -- * The setup hooks datatype
    SetupHooks (..)
  , noSetupHooks

    -- * Configure hooks
  , ConfigureHooks (..)
  , noConfigureHooks

    -- ** Per-package configure hooks
  , PreConfPackageInputs (..)
  , PreConfPackageOutputs (..)
  , noPreConfPackageOutputs
  , PreConfPackageHook
  , PostConfPackageInputs (..)
  , PostConfPackageHook

    -- ** Per-component configure hooks
  , PreConfComponentInputs (..)
  , PreConfComponentOutputs (..)
  , noPreConfComponentOutputs
  , PreConfComponentHook
  , ComponentDiff (..)
  , emptyComponentDiff
  , buildInfoComponentDiff
  , LibraryDiff
  , ForeignLibDiff
  , ExecutableDiff
  , TestSuiteDiff
  , BenchmarkDiff
  , BuildInfoDiff

    -- * Build hooks
  , BuildHooks (..)
  , noBuildHooks
  , BuildingWhat (..)
  , buildingWhatVerbosity
  , buildingWhatWorkingDir
  , buildingWhatDistPref

    -- ** Pre-build rules
  , PreBuildComponentInputs (..)
  , PreBuildComponentRules

    -- ** Post-build hook
  , PostBuildComponentInputs (..)
  , PostBuildComponentHook

    -- * Install hooks
  , InstallHooks (..)
  , noInstallHooks
  , InstallComponentInputs (..)
  , InstallComponentHook

    -- * Internals

    -- ** Per-component hook utilities
  , applyComponentDiffs
  , forComponents_

    -- ** Executing build rules
  , executeRules

    -- ** HookedBuildInfo compatibility code
  , hookedBuildInfoComponents
  , hookedBuildInfoComponentDiff_maybe
  )
where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Lens ((.~))
import Distribution.PackageDescription
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler (Compiler (..))
import Distribution.Simple.Errors
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Db
import Distribution.Simple.Setup
  ( BuildingWhat (..)
  , buildingWhatDistPref
  , buildingWhatVerbosity
  , buildingWhatWorkingDir
  )
import Distribution.Simple.Setup.Build (BuildFlags (..))
import Distribution.Simple.Setup.Config (ConfigFlags (..))
import Distribution.Simple.Setup.Copy (CopyFlags (..))
import Distribution.Simple.SetupHooks.Errors
import Distribution.Simple.SetupHooks.Rule
import qualified Distribution.Simple.SetupHooks.Rule as Rule
import Distribution.Simple.Utils
import Distribution.System (Platform (..))
import Distribution.Utils.Path

import qualified Distribution.Types.BuildInfo.Lens as BI (buildInfo)
import Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.TargetInfo
import Distribution.Verbosity

import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Graph as Graph
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Directory (doesFileExist)

--------------------------------------------------------------------------------
-- SetupHooks

-- | Hooks into the @cabal@ build phases.
--
-- Usage:
--
--  - In your @.cabal@ file, declare @build-type: Hooks@
--    (with a @cabal-version@ greater than or equal to @3.14@),
--  - In your @.cabal@ file, include a @custom-setup@ stanza
--    which declares the dependencies of your @SetupHooks@ module;
--    this will usually contain a dependency on the @Cabal-hooks@ package.
--  - Provide a @SetupHooks.hs@ module next to your @.cabal@ file;
--    it must export @setupHooks :: SetupHooks@.
data SetupHooks = SetupHooks
  { configureHooks :: ConfigureHooks
  -- ^ Hooks into the configure phase.
  , buildHooks :: BuildHooks
  -- ^ Hooks into the build phase.
  --
  -- These hooks are relevant to any build-like phase,
  -- such as repl or haddock.
  , installHooks :: InstallHooks
  -- ^ Hooks into the copy/install phase.
  }

-- | 'SetupHooks' can be combined monoidally. This is useful to combine
-- setup hooks defined by another package with your own package-specific
-- hooks.
--
-- __Warning__: this 'Semigroup' instance is not commutative.
instance Semigroup SetupHooks where
  SetupHooks
    { configureHooks = conf1
    , buildHooks = build1
    , installHooks = inst1
    }
    <> SetupHooks
      { configureHooks = conf2
      , buildHooks = build2
      , installHooks = inst2
      } =
      SetupHooks
        { configureHooks = conf1 <> conf2
        , buildHooks = build1 <> build2
        , installHooks = inst1 <> inst2
        }

instance Monoid SetupHooks where
  mempty = noSetupHooks

-- | Empty hooks.
noSetupHooks :: SetupHooks
noSetupHooks =
  SetupHooks
    { configureHooks = noConfigureHooks
    , buildHooks = noBuildHooks
    , installHooks = noInstallHooks
    }

--------------------------------------------------------------------------------
-- Configure hooks.

type PreConfPackageHook = PreConfPackageInputs -> IO PreConfPackageOutputs

-- | Inputs to the package-wide pre-configure step.
data PreConfPackageInputs = PreConfPackageInputs
  { configFlags :: ConfigFlags
  , localBuildConfig :: LocalBuildConfig
  -- ^ Warning: the 'ProgramDb' in the 'withPrograms' field
  -- will not contain any unconfigured programs.
  , compiler :: Compiler
  , platform :: Platform
  }
  deriving (Generic, Show)

-- | Outputs of the package-wide pre-configure step.
--
-- Prefer using 'noPreConfPackageOutputs' and overriding the fields
-- you care about, to avoid depending on implementation details
-- of this datatype.
data PreConfPackageOutputs = PreConfPackageOutputs
  { buildOptions :: BuildOptions
  , extraConfiguredProgs :: ConfiguredProgs
  }
  deriving (Generic, Show)

-- | Use this smart constructor to declare an empty set of changes
-- by the package-wide pre-configure hook, and override the fields you
-- care about.
--
-- Use this rather than v'PreConfPackageOutputs' to avoid relying on
-- internal implementation details of the latter.
noPreConfPackageOutputs :: PreConfPackageInputs -> PreConfPackageOutputs
noPreConfPackageOutputs (PreConfPackageInputs{localBuildConfig = lbc}) =
  PreConfPackageOutputs
    { buildOptions = LBC.withBuildOptions lbc
    , extraConfiguredProgs = Map.empty
    }

-- | Package-wide post-configure step.
--
-- Perform side effects. Last opportunity for any package-wide logic;
-- any subsequent hooks work per-component.
type PostConfPackageHook = PostConfPackageInputs -> IO ()

-- | Inputs to the package-wide post-configure step.
data PostConfPackageInputs = PostConfPackageInputs
  { localBuildConfig :: LocalBuildConfig
  , packageBuildDescr :: PackageBuildDescr
  }
  deriving (Generic, Show)

-- | Per-component pre-configure step.
--
-- For each component of the package, this hook can perform side effects,
-- and return a diff to the passed in component, e.g. to declare additional
-- autogenerated modules.
type PreConfComponentHook = PreConfComponentInputs -> IO PreConfComponentOutputs

-- | Inputs to the per-component pre-configure step.
data PreConfComponentInputs = PreConfComponentInputs
  { localBuildConfig :: LocalBuildConfig
  , packageBuildDescr :: PackageBuildDescr
  , component :: Component
  }
  deriving (Generic, Show)

-- | Outputs of the per-component pre-configure step.
--
-- Prefer using 'noPreComponentOutputs' and overriding the fields
-- you care about, to avoid depending on implementation details
-- of this datatype.
data PreConfComponentOutputs = PreConfComponentOutputs
  { componentDiff :: ComponentDiff
  }
  deriving (Generic, Show)

-- | Use this smart constructor to declare an empty set of changes
-- by a per-component pre-configure hook, and override the fields you
-- care about.
--
-- Use this rather than v'PreConfComponentOutputs' to avoid relying on
-- internal implementation details of the latter.
noPreConfComponentOutputs :: PreConfComponentInputs -> PreConfComponentOutputs
noPreConfComponentOutputs (PreConfComponentInputs{component = comp}) =
  PreConfComponentOutputs
    { componentDiff = emptyComponentDiff (componentName comp)
    }

-- | Configure-time hooks.
--
-- Order of execution:
--
--  - 'preConfPackageHook',
--  - configure the package,
--  - 'postConfPackageHook',
--  - 'preConfComponentHook',
--  - configure the components.
data ConfigureHooks = ConfigureHooks
  { preConfPackageHook :: Maybe PreConfPackageHook
  -- ^ Package-wide pre-configure hook. See 'PreConfPackageHook'.
  , postConfPackageHook :: Maybe PostConfPackageHook
  -- ^ Package-wide post-configure hook. See 'PostConfPackageHook'.
  , preConfComponentHook :: Maybe PreConfComponentHook
  -- ^ Per-component pre-configure hook. See 'PreConfComponentHook'.
  }

-- Note: these configure hooks don't track any kind of dependency information,
-- so we won't know when the configuration is out of date and should be re-done.
-- This seems okay: it should only matter while developing the package, in which
-- case it seems acceptable to rely on the user re-configuring.

instance Semigroup ConfigureHooks where
  ConfigureHooks
    { preConfPackageHook = prePkg1
    , postConfPackageHook = postPkg1
    , preConfComponentHook = preComp1
    }
    <> ConfigureHooks
      { preConfPackageHook = prePkg2
      , postConfPackageHook = postPkg2
      , preConfComponentHook = preComp2
      } =
      ConfigureHooks
        { preConfPackageHook =
            coerce
              ((<>) @(Maybe PreConfPkgSemigroup))
              prePkg1
              prePkg2
        , postConfPackageHook =
            postPkg1 <> postPkg2
        , preConfComponentHook =
            coerce
              ((<>) @(Maybe PreConfComponentSemigroup))
              preComp1
              preComp2
        }

instance Monoid ConfigureHooks where
  mempty = noConfigureHooks

-- | Empty configure phase hooks.
noConfigureHooks :: ConfigureHooks
noConfigureHooks =
  ConfigureHooks
    { preConfPackageHook = Nothing
    , postConfPackageHook = Nothing
    , preConfComponentHook = Nothing
    }

-- | A newtype to hang off the @Semigroup PreConfPackageHook@ instance.
newtype PreConfPkgSemigroup = PreConfPkgSemigroup PreConfPackageHook

instance Semigroup PreConfPkgSemigroup where
  PreConfPkgSemigroup f1 <> PreConfPkgSemigroup f2 =
    PreConfPkgSemigroup $
      \inputs@( PreConfPackageInputs
                  { configFlags = cfg
                  , compiler = comp
                  , platform = plat
                  , localBuildConfig = lbc0
                  }
                ) ->
          do
            PreConfPackageOutputs
              { buildOptions = opts1
              , extraConfiguredProgs = progs1
              } <-
              f1 inputs
            PreConfPackageOutputs
              { buildOptions = opts2
              , extraConfiguredProgs = progs2
              } <-
              f2 $
                PreConfPackageInputs
                  { configFlags = cfg
                  , compiler = comp
                  , platform = plat
                  , localBuildConfig =
                      lbc0
                        { LBC.withPrograms =
                            updateConfiguredProgs (`Map.union` progs1) $
                              LBC.withPrograms lbc0
                        , LBC.withBuildOptions = opts1
                        }
                  }
            return $
              PreConfPackageOutputs
                { buildOptions = opts2
                , extraConfiguredProgs = progs1 <> progs2
                }

-- | A newtype to hang off the @Semigroup PreConfComponentHook@ instance.
newtype PreConfComponentSemigroup = PreConfComponentSemigroup PreConfComponentHook

instance Semigroup PreConfComponentSemigroup where
  PreConfComponentSemigroup f1 <> PreConfComponentSemigroup f2 =
    PreConfComponentSemigroup $ \inputs ->
      do
        PreConfComponentOutputs
          { componentDiff = diff1
          } <-
          f1 inputs
        PreConfComponentOutputs
          { componentDiff = diff2
          } <-
          f2 inputs
        return $
          PreConfComponentOutputs
            { componentDiff = diff1 <> diff2
            }

--------------------------------------------------------------------------------
-- Build setup hooks.

data PreBuildComponentInputs = PreBuildComponentInputs
  { buildingWhat :: BuildingWhat
  -- ^ what kind of build phase are we hooking into?
  , localBuildInfo :: LocalBuildInfo
  -- ^ information about the package
  , targetInfo :: TargetInfo
  -- ^ information about an individual component
  }
  deriving (Generic, Show)

type PreBuildComponentRules = Rules PreBuildComponentInputs

data PostBuildComponentInputs = PostBuildComponentInputs
  { buildFlags :: BuildFlags
  , localBuildInfo :: LocalBuildInfo
  , targetInfo :: TargetInfo
  }
  deriving (Generic, Show)

type PostBuildComponentHook = PostBuildComponentInputs -> IO ()

-- | Build-time hooks.
data BuildHooks = BuildHooks
  { preBuildComponentRules :: Maybe PreBuildComponentRules
  -- ^ Per-component fine-grained pre-build rules.
  , postBuildComponentHook :: Maybe PostBuildComponentHook
  -- ^ Per-component post-build hook.
  }

-- Note that the pre-build hook consists of a function which takes a component
-- as an argument (as part of the targetInfo field) and returns a collection of
-- pre-build rules.
--
-- One might wonder why it isn't instead a collection of pre-build rules, one
-- for each component. The reason is that Backpack creates components on-the-fly
-- through instantiation, which means e.g. that a single component name can
-- resolve to multiple components. This means we really need to pass in the
-- components to the function, as we don't know the full details (e.g. their
-- unit ids) ahead of time.

instance Semigroup BuildHooks where
  BuildHooks
    { preBuildComponentRules = rs1
    , postBuildComponentHook = post1
    }
    <> BuildHooks
      { preBuildComponentRules = rs2
      , postBuildComponentHook = post2
      } =
      BuildHooks
        { preBuildComponentRules = rs1 <> rs2
        , postBuildComponentHook = post1 <> post2
        }

instance Monoid BuildHooks where
  mempty = noBuildHooks

-- | Empty build hooks.
noBuildHooks :: BuildHooks
noBuildHooks =
  BuildHooks
    { preBuildComponentRules = Nothing
    , postBuildComponentHook = Nothing
    }

--------------------------------------------------------------------------------
-- Install setup hooks.

data InstallComponentInputs = InstallComponentInputs
  { copyFlags :: CopyFlags
  , localBuildInfo :: LocalBuildInfo
  , targetInfo :: TargetInfo
  }
  deriving (Generic, Show)

-- | A per-component install hook,
-- which can only perform side effects (e.g. copying files).
type InstallComponentHook = InstallComponentInputs -> IO ()

-- | Copy/install hooks.
data InstallHooks = InstallHooks
  { installComponentHook :: Maybe InstallComponentHook
  -- ^ Per-component install hook.
  }

instance Semigroup InstallHooks where
  InstallHooks
    { installComponentHook = inst1
    }
    <> InstallHooks
      { installComponentHook = inst2
      } =
      InstallHooks
        { installComponentHook = inst1 <> inst2
        }

instance Monoid InstallHooks where
  mempty = noInstallHooks

-- | Empty copy/install hooks.
noInstallHooks :: InstallHooks
noInstallHooks =
  InstallHooks
    { installComponentHook = Nothing
    }

--------------------------------------------------------------------------------
-- Per-component configure hook implementation details.

type LibraryDiff = Library
type ForeignLibDiff = ForeignLib
type ExecutableDiff = Executable
type TestSuiteDiff = TestSuite
type BenchmarkDiff = Benchmark
type BuildInfoDiff = BuildInfo

-- | A diff to a Cabal 'Component', that gets combined monoidally into
-- an existing 'Component'.
newtype ComponentDiff = ComponentDiff {componentDiff :: Component}
  deriving (Semigroup, Show)

emptyComponentDiff :: ComponentName -> ComponentDiff
emptyComponentDiff name = ComponentDiff $
  case name of
    CLibName{} -> CLib emptyLibrary
    CFLibName{} -> CFLib emptyForeignLib
    CExeName{} -> CExe emptyExecutable
    CTestName{} -> CTest emptyTestSuite
    CBenchName{} -> CBench emptyBenchmark

buildInfoComponentDiff :: ComponentName -> BuildInfo -> ComponentDiff
buildInfoComponentDiff name bi = ComponentDiff $
  BI.buildInfo .~ bi $
    case name of
      CLibName{} -> CLib emptyLibrary
      CFLibName{} -> CFLib emptyForeignLib
      CExeName{} -> CExe emptyExecutable
      CTestName{} -> CTest emptyTestSuite
      CBenchName{} -> CBench emptyBenchmark

applyLibraryDiff :: Verbosity -> Library -> LibraryDiff -> IO Library
applyLibraryDiff verbosity lib diff =
  case illegalLibraryDiffReasons lib diff of
    [] -> return $ lib <> diff
    (r : rs) ->
      dieWithException verbosity $
        SetupHooksException $
          CannotApplyComponentDiff $
            IllegalComponentDiff (CLib lib) (r NE.:| rs)

illegalLibraryDiffReasons :: Library -> LibraryDiff -> [IllegalComponentDiffReason]
illegalLibraryDiffReasons
  lib
  Library
    { libName = nm
    , libExposed = e
    , libVisibility = vis
    , libBuildInfo = bi
    } =
    [ CannotChangeName
    | not $ nm == libName emptyLibrary || nm == libName lib
    ]
      ++ [ CannotChangeComponentField "libExposed"
         | not $ e == libExposed emptyLibrary || e == libExposed lib
         ]
      ++ [ CannotChangeComponentField "libVisibility"
         | not $ vis == libVisibility emptyLibrary || vis == libVisibility lib
         ]
      ++ illegalBuildInfoDiffReasons (libBuildInfo lib) bi

applyForeignLibDiff :: Verbosity -> ForeignLib -> ForeignLibDiff -> IO ForeignLib
applyForeignLibDiff verbosity flib diff =
  case illegalForeignLibDiffReasons flib diff of
    [] -> return $ flib <> diff
    (r : rs) ->
      dieWithException verbosity $
        SetupHooksException $
          CannotApplyComponentDiff $
            IllegalComponentDiff (CFLib flib) (r NE.:| rs)

illegalForeignLibDiffReasons :: ForeignLib -> ForeignLibDiff -> [IllegalComponentDiffReason]
illegalForeignLibDiffReasons
  flib
  ForeignLib
    { foreignLibName = nm
    , foreignLibType = ty
    , foreignLibOptions = opts
    , foreignLibVersionInfo = vi
    , foreignLibVersionLinux = linux
    , foreignLibModDefFile = defs
    , foreignLibBuildInfo = bi
    } =
    [ CannotChangeName
    | not $ nm == foreignLibName emptyForeignLib || nm == foreignLibName flib
    ]
      ++ [ CannotChangeComponentField "foreignLibType"
         | not $ ty == foreignLibType emptyForeignLib || ty == foreignLibType flib
         ]
      ++ [ CannotChangeComponentField "foreignLibOptions"
         | not $ opts == foreignLibOptions emptyForeignLib || opts == foreignLibOptions flib
         ]
      ++ [ CannotChangeComponentField "foreignLibVersionInfo"
         | not $ vi == foreignLibVersionInfo emptyForeignLib || vi == foreignLibVersionInfo flib
         ]
      ++ [ CannotChangeComponentField "foreignLibVersionLinux"
         | not $ linux == foreignLibVersionLinux emptyForeignLib || linux == foreignLibVersionLinux flib
         ]
      ++ [ CannotChangeComponentField "foreignLibModDefFile"
         | not $ defs == foreignLibModDefFile emptyForeignLib || defs == foreignLibModDefFile flib
         ]
      ++ illegalBuildInfoDiffReasons (foreignLibBuildInfo flib) bi

applyExecutableDiff :: Verbosity -> Executable -> ExecutableDiff -> IO Executable
applyExecutableDiff verbosity exe diff =
  case illegalExecutableDiffReasons exe diff of
    [] -> return $ exe <> diff
    (r : rs) ->
      dieWithException verbosity $
        SetupHooksException $
          CannotApplyComponentDiff $
            IllegalComponentDiff (CExe exe) (r NE.:| rs)

illegalExecutableDiffReasons :: Executable -> ExecutableDiff -> [IllegalComponentDiffReason]
illegalExecutableDiffReasons
  exe
  Executable
    { exeName = nm
    , modulePath = path
    , exeScope = scope
    , buildInfo = bi
    } =
    [ CannotChangeName
    | not $ nm == exeName emptyExecutable || nm == exeName exe
    ]
      ++ [ CannotChangeComponentField "modulePath"
         | not $ path == modulePath emptyExecutable || path == modulePath exe
         ]
      ++ [ CannotChangeComponentField "exeScope"
         | not $ scope == exeScope emptyExecutable || scope == exeScope exe
         ]
      ++ illegalBuildInfoDiffReasons (buildInfo exe) bi

applyTestSuiteDiff :: Verbosity -> TestSuite -> TestSuiteDiff -> IO TestSuite
applyTestSuiteDiff verbosity test diff =
  case illegalTestSuiteDiffReasons test diff of
    [] -> return $ test <> diff
    (r : rs) ->
      dieWithException verbosity $
        SetupHooksException $
          CannotApplyComponentDiff $
            IllegalComponentDiff (CTest test) (r NE.:| rs)

illegalTestSuiteDiffReasons :: TestSuite -> TestSuiteDiff -> [IllegalComponentDiffReason]
illegalTestSuiteDiffReasons
  test
  TestSuite
    { testName = nm
    , testInterface = iface
    , testCodeGenerators = gens
    , testBuildInfo = bi
    } =
    [ CannotChangeName
    | not $ nm == testName emptyTestSuite || nm == testName test
    ]
      ++ [ CannotChangeComponentField "testInterface"
         | not $ iface == testInterface emptyTestSuite || iface == testInterface test
         ]
      ++ [ CannotChangeComponentField "testCodeGenerators"
         | not $ gens == testCodeGenerators emptyTestSuite || gens == testCodeGenerators test
         ]
      ++ illegalBuildInfoDiffReasons (testBuildInfo test) bi

applyBenchmarkDiff :: Verbosity -> Benchmark -> BenchmarkDiff -> IO Benchmark
applyBenchmarkDiff verbosity bench diff =
  case illegalBenchmarkDiffReasons bench diff of
    [] -> return $ bench <> diff
    (r : rs) ->
      dieWithException verbosity $
        SetupHooksException $
          CannotApplyComponentDiff $
            IllegalComponentDiff (CBench bench) (r NE.:| rs)

illegalBenchmarkDiffReasons :: Benchmark -> BenchmarkDiff -> [IllegalComponentDiffReason]
illegalBenchmarkDiffReasons
  bench
  Benchmark
    { benchmarkName = nm
    , benchmarkInterface = iface
    , benchmarkBuildInfo = bi
    } =
    [ CannotChangeName
    | not $ nm == benchmarkName emptyBenchmark || nm == benchmarkName bench
    ]
      ++ [ CannotChangeComponentField "benchmarkInterface"
         | not $ iface == benchmarkInterface emptyBenchmark || iface == benchmarkInterface bench
         ]
      ++ illegalBuildInfoDiffReasons (benchmarkBuildInfo bench) bi

illegalBuildInfoDiffReasons :: BuildInfo -> BuildInfoDiff -> [IllegalComponentDiffReason]
illegalBuildInfoDiffReasons
  bi
  BuildInfo
    { buildable = can_build
    , buildTools = build_tools
    , buildToolDepends = build_tools_depends
    , pkgconfigDepends = pkgconfig_depends
    , frameworks = fworks
    , targetBuildDepends = target_build_depends
    } =
    map CannotChangeBuildInfoField $
      [ "buildable"
      | not $ can_build == buildable bi || can_build == buildable emptyBuildInfo
      ]
        ++ [ "buildTools"
           | not $ build_tools == buildTools bi || build_tools == buildTools emptyBuildInfo
           ]
        ++ [ "buildToolsDepends"
           | not $ build_tools_depends == buildToolDepends bi || build_tools_depends == buildToolDepends emptyBuildInfo
           ]
        ++ [ "pkgconfigDepends"
           | not $ pkgconfig_depends == pkgconfigDepends bi || pkgconfig_depends == pkgconfigDepends emptyBuildInfo
           ]
        ++ [ "frameworks"
           | not $ fworks == frameworks bi || fworks == frameworks emptyBuildInfo
           ]
        ++ [ "targetBuildDepends"
           | not $ target_build_depends == targetBuildDepends bi || target_build_depends == targetBuildDepends emptyBuildInfo
           ]

-- | Traverse the components of a 'PackageDescription'.
--
-- The function must preserve the component type, i.e. map a 'CLib' to a 'CLib',
-- a 'CExe' to a 'CExe', etc.
traverseComponents
  :: Applicative m
  => (Component -> m Component)
  -> PackageDescription
  -> m PackageDescription
traverseComponents f pd =
  upd_pd
    <$> traverse f_lib (library pd)
    <*> traverse f_lib (subLibraries pd)
    <*> traverse f_flib (foreignLibs pd)
    <*> traverse f_exe (executables pd)
    <*> traverse f_test (testSuites pd)
    <*> traverse f_bench (benchmarks pd)
  where
    f_lib lib = \case { CLib lib' -> lib'; c -> mismatch (CLib lib) c } <$> f (CLib lib)
    f_flib flib = \case { CFLib flib' -> flib'; c -> mismatch (CFLib flib) c } <$> f (CFLib flib)
    f_exe exe = \case { CExe exe' -> exe'; c -> mismatch (CExe exe) c } <$> f (CExe exe)
    f_test test = \case { CTest test' -> test'; c -> mismatch (CTest test) c } <$> f (CTest test)
    f_bench bench = \case { CBench bench' -> bench'; c -> mismatch (CBench bench) c } <$> f (CBench bench)

    upd_pd lib sublibs flibs exes tests benchs =
      pd
        { library = lib
        , subLibraries = sublibs
        , foreignLibs = flibs
        , executables = exes
        , testSuites = tests
        , benchmarks = benchs
        }

    -- This is a panic, because we maintain this invariant elsewhere:
    -- see 'componentDiffError' in 'applyComponentDiff', which catches an
    -- invalid per-component configure hook.
    mismatch c1 c2 =
      error $
        "Mismatched component types: "
          ++ showComponentName (componentName c1)
          ++ " "
          ++ showComponentName (componentName c2)
          ++ "."
{-# INLINEABLE traverseComponents #-}

applyComponentDiffs
  :: Verbosity
  -> (Component -> IO (Maybe ComponentDiff))
  -> PackageDescription
  -> IO PackageDescription
applyComponentDiffs verbosity f = traverseComponents apply_diff
  where
    apply_diff :: Component -> IO Component
    apply_diff c = do
      mbDiff <- f c
      case mbDiff of
        Just diff -> applyComponentDiff verbosity c diff
        Nothing -> return c

forComponents_ :: PackageDescription -> (Component -> IO ()) -> IO ()
forComponents_ pd f = getConst $ traverseComponents (Const . f) pd

applyComponentDiff
  :: Verbosity
  -> Component
  -> ComponentDiff
  -> IO Component
applyComponentDiff verbosity comp (ComponentDiff diff)
  | CLib lib <- comp
  , CLib lib_diff <- diff =
      CLib <$> applyLibraryDiff verbosity lib lib_diff
  | CFLib flib <- comp
  , CFLib flib_diff <- diff =
      CFLib <$> applyForeignLibDiff verbosity flib flib_diff
  | CExe exe <- comp
  , CExe exe_diff <- diff =
      CExe <$> applyExecutableDiff verbosity exe exe_diff
  | CTest test <- comp
  , CTest test_diff <- diff =
      CTest <$> applyTestSuiteDiff verbosity test test_diff
  | CBench bench <- comp
  , CBench bench_diff <- diff =
      CBench <$> applyBenchmarkDiff verbosity bench bench_diff
  | otherwise =
      componentDiffError $ MismatchedComponentTypes comp diff
  where
    -- The per-component configure hook specified a diff of the wrong type,
    -- e.g. tried to apply an executable diff to a library.
    componentDiffError err =
      dieWithException verbosity $
        SetupHooksException $
          CannotApplyComponentDiff err

--------------------------------------------------------------------------------
-- Running pre-build rules

-- | Run all pre-build rules.
--
-- This function should only be called internally within @Cabal@, as it is used
-- to implement the (legacy) Setup.hs interface. The build tool
-- (e.g. @cabal-install@ or @hls@) should instead go through the separate
-- hooks executable, which allows us to only rerun the out-of-date rules
-- (instead of running all of these rules at once).
executeRules
  :: Verbosity
  -> LocalBuildInfo
  -> TargetInfo
  -> Map RuleId Rule
  -> IO ()
executeRules =
  executeRulesUserOrSystem
    SUser
    (\_rId cmd -> sequenceA $ runRuleDynDepsCmd cmd)
    (\_rId cmd -> runRuleExecCmd cmd)

-- | Like 'executeRules', except it can be used when communicating with
-- an external hooks executable.
executeRulesUserOrSystem
  :: forall userOrSystem
   . SScope userOrSystem
  -> (RuleId -> RuleDynDepsCmd userOrSystem -> IO (Maybe ([Rule.Dependency], LBS.ByteString)))
  -> (RuleId -> RuleExecCmd userOrSystem -> IO ())
  -> Verbosity
  -> LocalBuildInfo
  -> TargetInfo
  -> Map RuleId (RuleData userOrSystem)
  -> IO ()
executeRulesUserOrSystem scope runDepsCmdData runCmdData verbosity lbi tgtInfo allRules = do
  -- Compute all extra dynamic dependency edges.
  dynDepsEdges <-
    flip Map.traverseMaybeWithKey allRules $
      \rId (Rule{ruleCommands = cmds}) ->
        runDepsCmdData rId (ruleDepsCmd cmds)

  -- Create a build graph of all the rules, with static and dynamic dependencies
  -- as edges.
  let
    (ruleGraph, ruleFromVertex, vertexFromRuleId) =
      Graph.graphFromEdges
        [ (rule, rId, nub $ mapMaybe directRuleDependencyMaybe allDeps)
        | (rId, rule) <- Map.toList allRules
        , let dynDeps = fromMaybe [] (fst <$> Map.lookup rId dynDepsEdges)
              allDeps = staticDependencies rule ++ dynDeps
        ]

    -- Topologically sort the graph of rules.
    sccs = Graph.scc ruleGraph
    cycles = mapMaybe $ \(Graph.Node v0 subforest) ->
      case subforest of
        []
          | r@(_, rId, deps) <- ruleFromVertex v0 ->
              if rId `elem` deps
                then Just (r, [])
                else Nothing
        v : vs ->
          Just
            ( ruleFromVertex v0
            , map (fmap ruleFromVertex) (v : vs)
            )

    -- Compute demanded rules.
    --
    -- SetupHooks TODO: maybe requiring all generated modules to appear
    -- in autogen-modules is excessive; we can look through all modules instead.
    autogenModPaths =
      map (\m -> moduleNameSymbolicPath m <.> "hs") $
        autogenModules $
          componentBuildInfo $
            targetComponent tgtInfo
    leafRule_maybe (rId, r) =
      if any ((r `ruleOutputsLocation`) . (Location compAutogenDir)) autogenModPaths
        then vertexFromRuleId rId
        else Nothing
    leafRules = mapMaybe leafRule_maybe $ Map.toList allRules
    demandedRuleVerts = Set.fromList $ concatMap (Graph.reachable ruleGraph) leafRules
    nonDemandedRuleVerts = Set.fromList (Graph.vertices ruleGraph) Set.\\ demandedRuleVerts

  case cycles sccs of
    -- If there are cycles in the dependency structure, don't execute
    -- any rules at all; just throw an error right off the bat.
    r : rs ->
      let getRule ((ru, _, _), js) = (toRuleBinary ru, fmap (fmap (\(rv, _, _) -> toRuleBinary rv)) js)
       in errorOut $
            CyclicRuleDependencies $
              fmap getRule (r NE.:| rs)
    -- Otherwise, run all the demanded rules in dependency order (in one go).
    -- (Fine-grained running of rules should happen in cabal-install or HLS,
    -- not in the Cabal library.)
    [] -> do
      -- Emit a warning if there are non-demanded rules.
      unless (null nonDemandedRuleVerts) $
        warn verbosity $
          unlines $
            "The following rules are not demanded and will not be run:"
              : concat
                [ [ "  - " ++ show rId ++ ","
                  , "    generating " ++ show (NE.toList $ results r)
                  ]
                | v <- Set.toList nonDemandedRuleVerts
                , let (r, rId, _) = ruleFromVertex v
                ]
              ++ [ "Possible reasons for this error:"
                 , "  - Some autogenerated modules were not declared"
                 , "    (in the package description or in the pre-configure hooks)"
                 , "  - The output location for an autogenerated module is incorrect,"
                 , "    (e.g. the file extension is incorrect, or"
                 , "     it is not in the appropriate 'autogenComponentModules' directory)"
                 ]

      -- Run all the demanded rules, in dependency order.
      for_ sccs $ \(Graph.Node ruleVertex _) ->
        -- Don't run a rule unless it is demanded.
        unless (ruleVertex `Set.member` nonDemandedRuleVerts) $ do
          let ( r@Rule
                  { ruleCommands = cmds
                  , staticDependencies = staticDeps
                  , results = reslts
                  }
                , rId
                , _staticRuleDepIds
                ) =
                  ruleFromVertex ruleVertex
              mbDyn = Map.lookup rId dynDepsEdges
              allDeps = staticDeps ++ fromMaybe [] (fst <$> mbDyn)
          -- Check that the dependencies the rule expects are indeed present.
          resolvedDeps <- traverse (resolveDependency verbosity rId allRules) allDeps
          missingRuleDeps <- filterM (missingDep mbWorkDir) resolvedDeps
          case NE.nonEmpty missingRuleDeps of
            Just missingDeps ->
              errorOut $ CantFindSourceForRuleDependencies (toRuleBinary r) missingDeps
            -- Dependencies OK: run the associated action.
            Nothing -> do
              let execCmd = ruleExecCmd scope cmds (snd <$> mbDyn)
              runCmdData rId execCmd
              -- Throw an error if running the action did not result in
              -- the generation of outputs that we expected it to.
              missingRuleResults <- filterM (missingDep mbWorkDir) $ NE.toList reslts
              for_ (NE.nonEmpty missingRuleResults) $ \missingResults ->
                errorOut $ MissingRuleOutputs (toRuleBinary r) missingResults
              return ()
  where
    toRuleBinary :: RuleData userOrSystem -> RuleBinary
    toRuleBinary = case scope of
      SUser -> ruleBinary
      SSystem -> id
    clbi = targetCLBI tgtInfo
    mbWorkDir = mbWorkDirLBI lbi
    compAutogenDir = autogenComponentModulesDir lbi clbi
    errorOut e =
      dieWithException verbosity $
        SetupHooksException $
          RulesException e

directRuleDependencyMaybe :: Rule.Dependency -> Maybe RuleId
directRuleDependencyMaybe (RuleDependency dep) = Just $ outputOfRule dep
directRuleDependencyMaybe (FileDependency{}) = Nothing

resolveDependency :: Verbosity -> RuleId -> Map RuleId (RuleData scope) -> Rule.Dependency -> IO Location
resolveDependency verbosity rId allRules = \case
  FileDependency l -> return l
  RuleDependency (RuleOutput{outputOfRule = depId, outputIndex = i}) ->
    case Map.lookup depId allRules of
      Nothing ->
        error $
          unlines $
            [ "Internal error: missing rule dependency."
            , "Rule: " ++ show rId
            , "Dependency: " ++ show depId
            ]
      Just (Rule{results = os}) ->
        let j :: Int
            j = fromIntegral i
         in case listToMaybe $ drop j $ NE.toList os of
              Just o
                | j >= 0 ->
                    return o
              _ ->
                dieWithException verbosity $
                  SetupHooksException $
                    RulesException $
                      InvalidRuleOutputIndex rId depId os i

-- | Does the rule output the given location?
ruleOutputsLocation :: RuleData scope -> Location -> Bool
ruleOutputsLocation (Rule{results = rs}) fp =
  any (\out -> normaliseLocation out == normaliseLocation fp) rs

normaliseLocation :: Location -> Location
normaliseLocation (Location base rel) =
  Location (normaliseSymbolicPath base) (normaliseSymbolicPath rel)

-- | Is the file we depend on missing?
missingDep :: Maybe (SymbolicPath CWD (Dir Pkg)) -> Location -> IO Bool
missingDep mbWorkDir loc = not <$> doesFileExist fp
  where
    fp = interpretSymbolicPath mbWorkDir (location loc)

--------------------------------------------------------------------------------
-- Compatibility with HookedBuildInfo.
--
-- NB: assumes that the components in HookedBuildInfo are:
--  - an (optional) main library,
--  - executables.
--
-- No support for named sublibraries, foreign libraries, tests or benchmarks,
-- because the HookedBuildInfo datatype doesn't specify what type of component
-- each component name is (so we assume they are executables).

hookedBuildInfoComponents :: HookedBuildInfo -> Set ComponentName
hookedBuildInfoComponents (mb_mainlib, exes) =
  Set.fromList $
    (case mb_mainlib of Nothing -> id; Just{} -> (CLibName LMainLibName :))
      [CExeName exe_nm | (exe_nm, _) <- exes]

hookedBuildInfoComponentDiff_maybe :: HookedBuildInfo -> ComponentName -> Maybe (IO ComponentDiff)
hookedBuildInfoComponentDiff_maybe (mb_mainlib, exes) comp_nm =
  case comp_nm of
    CLibName lib_nm ->
      case lib_nm of
        LMainLibName -> return . ComponentDiff . CLib . buildInfoLibraryDiff <$> mb_mainlib
        LSubLibName{} -> Nothing
    CExeName exe_nm ->
      let mb_exe = lookup exe_nm exes
       in return . ComponentDiff . CExe . buildInfoExecutableDiff <$> mb_exe
    CFLibName{} -> Nothing
    CTestName{} -> Nothing
    CBenchName{} -> Nothing

buildInfoLibraryDiff :: BuildInfo -> LibraryDiff
buildInfoLibraryDiff bi = emptyLibrary{libBuildInfo = bi}

buildInfoExecutableDiff :: BuildInfo -> ExecutableDiff
buildInfoExecutableDiff bi = emptyExecutable{buildInfo = bi}

--------------------------------------------------------------------------------
-- Instances for serialisation

deriving newtype instance Binary ComponentDiff
deriving newtype instance Structured ComponentDiff

instance Binary PreConfPackageInputs
instance Structured PreConfPackageInputs
instance Binary PreConfPackageOutputs
instance Structured PreConfPackageOutputs

instance Binary PostConfPackageInputs
instance Structured PostConfPackageInputs

instance Binary PreConfComponentInputs
instance Structured PreConfComponentInputs
instance Binary PreConfComponentOutputs
instance Structured PreConfComponentOutputs

instance Binary PreBuildComponentInputs
instance Structured PreBuildComponentInputs

instance Binary PostBuildComponentInputs
instance Structured PostBuildComponentInputs

instance Binary InstallComponentInputs
instance Structured InstallComponentInputs

--------------------------------------------------------------------------------
