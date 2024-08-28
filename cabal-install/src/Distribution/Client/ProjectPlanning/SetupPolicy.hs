{-# LANGUAGE TypeFamilies #-}

-- | Setup.hs script policy
--
-- Handling for Setup.hs scripts is a bit tricky, part of it lives in the
-- solver phase, and part in the elaboration phase. We keep the helper
-- functions for both phases together here so at least you can see all of it
-- in one place.
--
-- There are four major cases for Setup.hs handling:
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
-- while in case 4 we can use the internal library API.
--
-- @since 3.12.0.0
module Distribution.Client.ProjectPlanning.SetupPolicy
  ( mkDefaultSetupDeps
  , packageSetupScriptStyle
  , packageSetupScriptSpecVersion
  , NonSetupLibDepSolverPlanPackage (..)
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ProjectPlanning.Types (SetupScriptStyle (..))
import Distribution.Client.SolverInstallPlan (SolverPlanPackage)
import Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.ResolverPackage (resolverPackageLibDeps)
import Distribution.Solver.Types.SolverId (SolverId)

import Distribution.CabalSpecVersion

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Compiler
import Distribution.System

import Distribution.Simple.Utils
import Distribution.Version

import Distribution.Compat.Graph (IsNode (..))
import qualified Distribution.Compat.Graph as Graph

-- | Work out the 'SetupScriptStyle' given the package description.
--
-- @since 3.12.0.0
packageSetupScriptStyle :: PackageDescription -> SetupScriptStyle
packageSetupScriptStyle pkg
  | customOrHooks
  , Just setupbi <- setupBuildInfo pkg -- does have a custom-setup stanza
  , not (defaultSetupDepends setupbi) -- but not one we added ourselves
    =
      SetupCustomExplicitDeps
  | customOrHooks
  , Just setupbi <- setupBuildInfo pkg -- does have a custom-setup stanza
  , defaultSetupDepends setupbi -- that we had to add ourselves
    =
      SetupCustomImplicitDeps
  | customOrHooks
  , Nothing <- setupBuildInfo pkg -- we get this case pre-solver
    =
      SetupCustomImplicitDeps
  -- The specified @cabal-version@ is newer that the last we know about.
  -- Here we could fail but we are optimist and build an external setup script.
  | specVersion pkg > cabalSpecLatest =
      SetupNonCustomExternalLib
  | otherwise =
      SetupNonCustomInternalLib
  where
    customOrHooks = buildType pkg `elem` [Custom, Hooks]

-- | Part of our Setup.hs handling policy is implemented by getting the solver
-- to work out setup dependencies for packages. The solver already handles
-- packages that explicitly specify setup dependencies, but we can also tell
-- the solver to treat other packages as if they had setup dependencies.
-- That's what this function does, it gets called by 'planPackages' for all
-- packages that don't already have setup dependencies.
--
-- The dependencies we want to add is different for each 'SetupScriptStyle'.
--
-- Note in addition to adding setup dependencies, we also use
-- 'addSetupCabalMinVersionConstraint' (in 'planPackages') to require
-- @Cabal >= 1.20@ for Setup scripts.
--
-- @since 3.12.0.0
mkDefaultSetupDeps
  :: Compiler
  -> Platform
  -> PackageDescription
  -> Maybe [Dependency]
mkDefaultSetupDeps compiler platform pkg =
  case packageSetupScriptStyle pkg of
    -- For packages with build type custom that do not specify explicit
    -- setup dependencies, we add a dependency on Cabal and a number
    -- of other packages.
    SetupCustomImplicitDeps ->
      Just $
        [ Dependency depPkgname anyVersion mainLibSet
        | depPkgname <- legacyCustomSetupPkgs compiler platform
        ]
          ++ [ Dependency cabalPkgname cabalConstraint mainLibSet
             | packageName pkg /= cabalPkgname
             ]
      where
        -- The Cabal dep is slightly special:
        -- \* We omit the dep for the Cabal lib itself, since it bootstraps.
        -- \* We constrain it to be < 1.25
        --
        -- Note: we also add a global constraint to require Cabal >= 1.20
        -- for Setup scripts (see use addSetupCabalMinVersionConstraint).
        --
        cabalConstraint =
          orLaterVersion (csvToVersion (specVersion pkg))
            `intersectVersionRanges` earlierVersion cabalCompatMaxVer
        -- The idea here is that at some point we will make significant
        -- breaking changes to the Cabal API that Setup.hs scripts use.
        -- So for old custom Setup scripts that do not specify explicit
        -- constraints, we constrain them to use a compatible Cabal version.
        cabalCompatMaxVer = mkVersion [1, 25]

    -- For other build types (like Simple) if we still need to compile an
    -- external Setup.hs, it'll be one of the simple ones that only depends
    -- on Cabal.
    SetupNonCustomExternalLib ->
      Just
        [ Dependency cabalPkgname cabalConstraint mainLibSet
        ]
      where
        cabalConstraint = orLaterVersion (csvToVersion (specVersion pkg))

    -- The internal setup wrapper method has no deps at all.
    SetupNonCustomInternalLib -> Just []
    -- This case gets ruled out by the caller, planPackages, see the note
    -- above in the SetupCustomImplicitDeps case.
    SetupCustomExplicitDeps ->
      error $
        "mkDefaultSetupDeps: called for a package with explicit "
          ++ "setup deps: "
          ++ prettyShow (packageId pkg)
  where
    -- we require one less
    --
    -- This maps e.g. CabalSpecV3_0 to mkVersion [2,5]
    csvToVersion :: CabalSpecVersion -> Version
    csvToVersion = mkVersion . cabalSpecMinimumLibraryVersion

-- | A newtype for 'SolverPlanPackage' for which the
-- dependency graph considers only dependencies on libraries which are
-- NOT from setup dependencies. Used to compute the set
-- of packages needed for profiling and dynamic libraries.
--
-- @since 3.12.0.0
newtype NonSetupLibDepSolverPlanPackage = NonSetupLibDepSolverPlanPackage
  {unNonSetupLibDepSolverPlanPackage :: SolverPlanPackage}

instance Package NonSetupLibDepSolverPlanPackage where
  packageId (NonSetupLibDepSolverPlanPackage spkg) =
    packageId spkg

instance IsNode NonSetupLibDepSolverPlanPackage where
  type Key NonSetupLibDepSolverPlanPackage = SolverId

  nodeKey (NonSetupLibDepSolverPlanPackage spkg) =
    nodeKey spkg

  nodeNeighbors (NonSetupLibDepSolverPlanPackage spkg) =
    ordNub $ CD.nonSetupDeps (resolverPackageLibDeps spkg)

-- | Work out which version of the Cabal we will be using to talk to the
-- Setup.hs interface for this package.
--
-- This depends somewhat on the 'SetupScriptStyle' but most cases are a result
-- of what the solver picked for us, based on the explicit setup deps or the
-- ones added implicitly by 'mkDefaultSetupDeps'.
--
-- @since 3.12.0.0
packageSetupScriptSpecVersion
  :: SetupScriptStyle
  -> PackageDescription
  -> Graph.Graph NonSetupLibDepSolverPlanPackage
  -> ComponentDeps [SolverId]
  -> Version
-- We're going to be using the internal Cabal library, so the spec version of
-- that is simply the version of the Cabal library that cabal-install has been
-- built with.
packageSetupScriptSpecVersion SetupNonCustomInternalLib _ _ _ =
  cabalVersion
-- If we happen to be building the Cabal lib itself then because that
-- bootstraps itself then we use the version of the lib we're building.
packageSetupScriptSpecVersion SetupCustomImplicitDeps pkg _ _
  | packageName pkg == cabalPkgname =
      packageVersion pkg
-- In all other cases we have a look at what version of the Cabal lib the
-- solver picked. Or if it didn't depend on Cabal at all (which is very rare)
-- then we look at the .cabal file to see what spec version it declares.
packageSetupScriptSpecVersion _ pkg libDepGraph deps =
  case find ((cabalPkgname ==) . packageName) setupLibDeps of
    Just dep -> packageVersion dep
    Nothing -> mkVersion (cabalSpecMinimumLibraryVersion (specVersion pkg))
  where
    setupLibDeps =
      map packageId $
        fromMaybe [] $
          Graph.closure libDepGraph (CD.setupDeps deps)

cabalPkgname :: PackageName
cabalPkgname = mkPackageName "Cabal"

legacyCustomSetupPkgs :: Compiler -> Platform -> [PackageName]
legacyCustomSetupPkgs compiler (Platform _ os) =
  map mkPackageName $
    [ "array"
    , "base"
    , "binary"
    , "bytestring"
    , "containers"
    , "deepseq"
    , "directory"
    , "filepath"
    , "pretty"
    , "process"
    , "time"
    , "transformers"
    ]
      ++ ["Win32" | os == Windows]
      ++ ["unix" | os /= Windows]
      ++ ["ghc-prim" | isGHC]
      ++ ["template-haskell" | isGHC]
      ++ ["old-time" | notGHC710]
  where
    isGHC = compilerCompatFlavor GHC compiler
    notGHC710 = case compilerCompatVersion GHC compiler of
      Nothing -> False
      Just v -> v <= mkVersion [7, 9]
