{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Additions to the components of a package description.
module Distribution.Types.ComponentDiff
  ( ComponentDiff (..)
  , emptyComponentDiff
  , buildInfoComponentDiff
  , addComponentDiffs
  , traverseComponents
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Lens ((.~))
import Distribution.PackageDescription
import qualified Distribution.Types.BuildInfo.Lens as BI (buildInfo)
import Distribution.Types.Component (Component (..), componentName)

import qualified Data.Map as Map

-- | A diff to a Cabal 'Component', that gets combined monoidally into
-- an existing 'Component'.
newtype ComponentDiff = ComponentDiff {componentDiff :: Component}
  deriving stock (Read, Show)
  deriving newtype (Semigroup, Binary, Structured)

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

-- | Combine each diff into the component of the package description with
-- that name.
--
-- A diff must be of the same type as the component it applies to: a library
-- diff for a library, an executable diff for an executable, etc.
addComponentDiffs
  :: Map ComponentName ComponentDiff
  -> PackageDescription
  -> PackageDescription
addComponentDiffs diffs
  | Map.null diffs = id
  | otherwise = runIdentity . traverseComponents (Identity . add_diff)
  where
    add_diff :: Component -> Component
    add_diff c =
      case Map.lookup (componentName c) diffs of
        Nothing -> c
        Just (ComponentDiff diff) -> c <> diff

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
