{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.InstallPlan
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Package installation plan
--
-----------------------------------------------------------------------------
module Distribution.Client.InstallPlan (
  InstallPlan,
  GenericInstallPlan,
  PlanPackage,
  GenericPlanPackage(..),

  -- * Operations on 'InstallPlan's
  new,
  toList,

  fromSolverInstallPlan,
  configureInstallPlan,

  ready,
  processing,
  completed,
  failed,
  remove,
  preexisting,
  preinstalled,

  -- * Traversal
  executionOrder,
  execute,
  BuildResults,
  lookupBuildResult,
  -- ** Traversal helpers
  -- $traversal
  Processing,
  ready',
  completed',
  failed',

  -- * Display
  showPlanIndex,
  showInstallPlan,

  -- * Graph-like operations
  reverseTopologicalOrder,
  ) where

import Distribution.Client.Types
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Simple.Setup as Cabal

import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( PackageIdentifier(..), Package(..)
         , HasUnitId(..), UnitId(..) )
import Distribution.Solver.Types.SolverPackage
import Distribution.Client.JobControl
import Distribution.Text
         ( display )
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.PackageFixedDeps
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SolverId

-- TODO: Need this when we compute final UnitIds
-- import qualified Distribution.Simple.Configure as Configure

import Data.List
         ( foldl', intercalate )
import Data.Maybe
         ( fromMaybe, catMaybes, isJust )
import qualified Distribution.Compat.Graph as Graph
import Distribution.Compat.Graph (Graph, IsNode(..))
import Distribution.Compat.Binary (Binary(..))
import GHC.Generics
import Control.Monad
import Control.Exception
         ( assert )
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Traversable as T


-- When cabal tries to install a number of packages, including all their
-- dependencies it has a non-trivial problem to solve.
--
-- The Problem:
--
-- In general we start with a set of installed packages and a set of source
-- packages.
--
-- Installed packages have fixed dependencies. They have already been built and
-- we know exactly what packages they were built against, including their exact
-- versions.
--
-- Source package have somewhat flexible dependencies. They are specified as
-- version ranges, though really they're predicates. To make matters worse they
-- have conditional flexible dependencies. Configuration flags can affect which
-- packages are required and can place additional constraints on their
-- versions.
--
-- These two sets of package can and usually do overlap. There can be installed
-- packages that are also available as source packages which means they could
-- be re-installed if required, though there will also be packages which are
-- not available as source and cannot be re-installed. Very often there will be
-- extra versions available than are installed. Sometimes we may like to prefer
-- installed packages over source ones or perhaps always prefer the latest
-- available version whether installed or not.
--
-- The goal is to calculate an installation plan that is closed, acyclic and
-- consistent and where every configured package is valid.
--
-- An installation plan is a set of packages that are going to be used
-- together. It will consist of a mixture of installed packages and source
-- packages along with their exact version dependencies. An installation plan
-- is closed if for every package in the set, all of its dependencies are
-- also in the set. It is consistent if for every package in the set, all
-- dependencies which target that package have the same version.

-- Note that plans do not necessarily compose. You might have a valid plan for
-- package A and a valid plan for package B. That does not mean the composition
-- is simultaneously valid for A and B. In particular you're most likely to
-- have problems with inconsistent dependencies.
-- On the other hand it is true that every closed sub plan is valid.

-- | Packages in an install plan
--
-- NOTE: 'ConfiguredPackage', 'GenericReadyPackage' and 'GenericPlanPackage'
-- intentionally have no 'PackageInstalled' instance. `This is important:
-- PackageInstalled returns only library dependencies, but for package that
-- aren't yet installed we know many more kinds of dependencies (setup
-- dependencies, exe, test-suite, benchmark, ..). Any functions that operate on
-- dependencies in cabal-install should consider what to do with these
-- dependencies; if we give a 'PackageInstalled' instance it would be too easy
-- to get this wrong (and, for instance, call graph traversal functions from
-- Cabal rather than from cabal-install). Instead, see 'PackageFixedDeps'.
data GenericPlanPackage ipkg srcpkg iresult ifailure
   = PreExisting ipkg
   | Configured  srcpkg
   | Processing  (GenericReadyPackage srcpkg)
   | Installed   (GenericReadyPackage srcpkg) (Maybe ipkg) iresult
   | Failed      srcpkg ifailure
  deriving (Eq, Show, Generic)

instance (HasUnitId ipkg,   PackageFixedDeps ipkg,
          HasUnitId srcpkg, PackageFixedDeps srcpkg)
         => IsNode (GenericPlanPackage ipkg srcpkg iresult ifailure) where
    type Key (GenericPlanPackage ipkg srcpkg iresult ifailure) = UnitId -- TODO: change me
    nodeKey = installedUnitId
    nodeNeighbors = CD.flatDeps . depends

instance (Binary ipkg, Binary srcpkg, Binary  iresult, Binary  ifailure)
      => Binary (GenericPlanPackage ipkg srcpkg iresult ifailure)

type PlanPackage = GenericPlanPackage
                   InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)
                   BuildSuccess BuildFailure

instance (Package ipkg, Package srcpkg) =>
         Package (GenericPlanPackage ipkg srcpkg iresult ifailure) where
  packageId (PreExisting ipkg)     = packageId ipkg
  packageId (Configured  spkg)     = packageId spkg
  packageId (Processing  rpkg)     = packageId rpkg
  packageId (Installed   rpkg _ _) = packageId rpkg
  packageId (Failed      spkg   _) = packageId spkg

instance (PackageFixedDeps srcpkg,
          PackageFixedDeps ipkg) =>
         PackageFixedDeps (GenericPlanPackage ipkg srcpkg iresult ifailure) where
  depends (PreExisting pkg)     = depends pkg
  depends (Configured  pkg)     = depends pkg
  depends (Processing  pkg)     = depends pkg
  depends (Installed   pkg _ _) = depends pkg
  depends (Failed      pkg   _) = depends pkg

instance (HasUnitId ipkg, HasUnitId srcpkg) =>
         HasUnitId
         (GenericPlanPackage ipkg srcpkg iresult ifailure) where
  installedUnitId (PreExisting ipkg ) = installedUnitId ipkg
  installedUnitId (Configured  spkg)  = installedUnitId spkg
  installedUnitId (Processing  rpkg)  = installedUnitId rpkg
  -- NB: defer to the actual installed package info in this case
  installedUnitId (Installed _ (Just ipkg) _) = installedUnitId ipkg
  installedUnitId (Installed rpkg _        _) = installedUnitId rpkg
  installedUnitId (Failed      spkg        _) = installedUnitId spkg

data GenericInstallPlan ipkg srcpkg iresult ifailure = GenericInstallPlan {
    planIndex      :: !(PlanIndex ipkg srcpkg iresult ifailure),
    planIndepGoals :: !IndependentGoals
  }

-- | 'GenericInstallPlan' specialised to most commonly used types.
type InstallPlan = GenericInstallPlan
                   InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)
                   BuildSuccess BuildFailure

type PlanIndex ipkg srcpkg iresult ifailure =
     Graph (GenericPlanPackage ipkg srcpkg iresult ifailure)

invariant :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
              HasUnitId srcpkg, PackageFixedDeps srcpkg)
          => GenericInstallPlan ipkg srcpkg iresult ifailure -> Bool
invariant plan =
    valid (planIndepGoals plan)
          (planIndex plan)

-- | Smart constructor that deals with caching the 'Graph' representation.
--
mkInstallPlan :: PlanIndex ipkg srcpkg iresult ifailure
              -> IndependentGoals
              -> GenericInstallPlan ipkg srcpkg iresult ifailure
mkInstallPlan index indepGoals =
    GenericInstallPlan {
      planIndex      = index,
      planIndepGoals = indepGoals
    }

internalError :: String -> a
internalError msg = error $ "InstallPlan: internal error: " ++ msg

instance (HasUnitId ipkg,   PackageFixedDeps ipkg,
          HasUnitId srcpkg, PackageFixedDeps srcpkg,
          Binary ipkg, Binary srcpkg, Binary iresult, Binary ifailure)
       => Binary (GenericInstallPlan ipkg srcpkg iresult ifailure) where
    put GenericInstallPlan {
              planIndex      = index,
              planIndepGoals = indepGoals
        } = put (index, indepGoals)

    get = do
      (index, indepGoals) <- get
      return $! mkInstallPlan index indepGoals

showPlanIndex :: (HasUnitId ipkg, HasUnitId srcpkg)
              => PlanIndex ipkg srcpkg iresult ifailure -> String
showPlanIndex index =
    intercalate "\n" (map showPlanPackage (Graph.toList index))
  where showPlanPackage p =
            showPlanPackageTag p ++ " "
                ++ display (packageId p) ++ " ("
                ++ display (installedUnitId p) ++ ")"

showInstallPlan :: (HasUnitId ipkg, HasUnitId srcpkg)
                => GenericInstallPlan ipkg srcpkg iresult ifailure -> String
showInstallPlan = showPlanIndex . planIndex

showPlanPackageTag :: GenericPlanPackage ipkg srcpkg iresult ifailure -> String
showPlanPackageTag (PreExisting _)   = "PreExisting"
showPlanPackageTag (Configured  _)   = "Configured"
showPlanPackageTag (Processing  _)   = "Processing"
showPlanPackageTag (Installed _ _ _) = "Installed"
showPlanPackageTag (Failed    _   _) = "Failed"

-- | Build an installation plan from a valid set of resolved packages.
--
new :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
        HasUnitId srcpkg, PackageFixedDeps srcpkg)
    => IndependentGoals
    -> PlanIndex ipkg srcpkg iresult ifailure
    -> Either [PlanProblem ipkg srcpkg iresult ifailure]
              (GenericInstallPlan ipkg srcpkg iresult ifailure)
new indepGoals index =
  case problems indepGoals index of
    []    -> Right (mkInstallPlan index indepGoals)
    probs -> Left probs

toList :: GenericInstallPlan ipkg srcpkg iresult ifailure
       -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
toList = Graph.toList . planIndex

-- | Remove packages from the install plan. This will result in an
-- error if there are remaining packages that depend on any matching
-- package. This is primarily useful for obtaining an install plan for
-- the dependencies of a package or set of packages without actually
-- installing the package itself, as when doing development.
--
remove :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
           HasUnitId srcpkg, PackageFixedDeps srcpkg)
       => (GenericPlanPackage ipkg srcpkg iresult ifailure -> Bool)
       -> GenericInstallPlan ipkg srcpkg iresult ifailure
       -> Either [PlanProblem ipkg srcpkg iresult ifailure]
                 (GenericInstallPlan ipkg srcpkg iresult ifailure)
remove shouldRemove plan =
    new (planIndepGoals plan) newIndex
  where
    newIndex = Graph.fromList $
                 filter (not . shouldRemove) (toList plan)

-- | The packages that are ready to be installed. That is they are in the
-- configured state and have all their dependencies installed already.
-- The plan is complete if the result is @[]@.
--
ready :: forall ipkg srcpkg iresult ifailure.
         (HasUnitId ipkg,   PackageFixedDeps ipkg,
          HasUnitId srcpkg, PackageFixedDeps srcpkg)
      => GenericInstallPlan ipkg srcpkg iresult ifailure
      -> [GenericReadyPackage srcpkg]
ready plan = assert check readyPackages
  where
    check = if null readyPackages && null processingPackages
              then null configuredPackages
              else True
    configuredPackages = [ pkg | Configured pkg <- toList plan ]
    processingPackages = [ pkg | Processing pkg <- toList plan]

    readyPackages :: [GenericReadyPackage srcpkg]
    readyPackages = catMaybes (map (lookupReadyPackage plan) configuredPackages)

lookupReadyPackage :: forall ipkg srcpkg iresult ifailure.
                      (HasUnitId ipkg,   PackageFixedDeps ipkg,
                       HasUnitId srcpkg, PackageFixedDeps srcpkg)
                   => GenericInstallPlan ipkg srcpkg iresult ifailure
                   -> srcpkg
                   -> Maybe (GenericReadyPackage srcpkg)
lookupReadyPackage plan pkg = do
    _ <- hasAllInstalledDeps pkg
    return (ReadyPackage pkg)
  where

    hasAllInstalledDeps :: srcpkg -> Maybe (ComponentDeps [ipkg])
    hasAllInstalledDeps = T.mapM (mapM isInstalledDep) . depends

    isInstalledDep :: UnitId -> Maybe ipkg
    isInstalledDep pkgid =
      case Graph.lookup pkgid (planIndex plan) of
        Just (PreExisting ipkg)            -> Just ipkg
        Just (Configured  _)               -> Nothing
        Just (Processing  _)               -> Nothing
        Just (Installed   _ (Just ipkg) _) -> Just ipkg
        Just (Installed   _ Nothing     _) -> internalError (depOnNonLib pkgid)
        Just (Failed      _             _) -> internalError depOnFailed
        Nothing                            -> internalError incomplete
    incomplete  = "install plan is not closed"
    depOnFailed = "configured package depends on failed package"
    depOnNonLib dep = "the configured package "
                   ++ display (packageId pkg)
                   ++ " depends on a non-library package "
                   ++ display dep

-- | Marks packages in the graph as currently processing (e.g. building).
--
-- * The package must exist in the graph and be in the configured state.
--
processing :: forall ipkg srcpkg iresult ifailure.
              (HasUnitId ipkg,   PackageFixedDeps ipkg,
               HasUnitId srcpkg, PackageFixedDeps srcpkg)
           => [GenericReadyPackage srcpkg]
           -> GenericInstallPlan ipkg srcpkg iresult ifailure
           -> GenericInstallPlan ipkg srcpkg iresult ifailure
processing pkgs plan = assert (invariant plan') plan'
  where
    plan' = plan {
              planIndex = Graph.unionRight (planIndex plan) processingPkgs
            }
    processingPkgs :: PlanIndex ipkg srcpkg iresult ifailure
    processingPkgs = Graph.fromList [Processing pkg | pkg <- pkgs]

-- | Marks a package in the graph as completed. Also saves the build result for
-- the completed package in the plan.
--
-- * The package must exist in the graph and be in the processing state.
-- * The package must have had no uninstalled dependent packages.
--
completed :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
              HasUnitId srcpkg, PackageFixedDeps srcpkg)
          => UnitId
          -> Maybe ipkg -> iresult
          -> GenericInstallPlan ipkg srcpkg iresult ifailure
          -> GenericInstallPlan ipkg srcpkg iresult ifailure
completed pkgid mipkg buildResult plan = assert (invariant plan') plan'
  where
    plan'     = plan {
                  planIndex = Graph.insert installed
                            . Graph.deleteKey pkgid
                            $ planIndex plan
                }
    installed = Installed (lookupProcessingPackage plan pkgid) mipkg buildResult

-- | Marks a package in the graph as having failed. It also marks all the
-- packages that depended on it as having failed.
--
-- * The package must exist in the graph and be in the processing
-- state.
--
failed :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
           HasUnitId srcpkg, PackageFixedDeps srcpkg)
       => UnitId         -- ^ The id of the package that failed to install
       -> ifailure           -- ^ The build result to use for the failed package
       -> ifailure           -- ^ The build result to use for its dependencies
       -> GenericInstallPlan ipkg srcpkg iresult ifailure
       -> GenericInstallPlan ipkg srcpkg iresult ifailure
failed pkgid buildResult buildResult' plan = assert (invariant plan') plan'
  where
    -- NB: failures don't update IPIDs
    plan'    = plan {
                 planIndex = Graph.unionRight (planIndex plan) failures
               }
    ReadyPackage srcpkg = lookupProcessingPackage plan pkgid
    failures = Graph.fromList
             $ Failed srcpkg buildResult
             : [ Failed pkg' buildResult'
               | Just pkg' <- map checkConfiguredPackage
                            $ packagesThatDependOn plan pkgid ]

-- | Lookup the reachable packages in the reverse dependency graph.
-- Does NOT include the package for @pkgid@!
--
packagesThatDependOn :: (HasUnitId ipkg, HasUnitId srcpkg)
                     => GenericInstallPlan ipkg srcpkg iresult ifailure
                     -> UnitId
                     -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
packagesThatDependOn plan pkgid = filter ((/= pkgid) . installedUnitId)
                                $ case Graph.revClosure (planIndex plan) [pkgid] of
                                    Nothing -> []
                                    Just r -> r

-- | Lookup a package that we expect to be in the processing state.
--
lookupProcessingPackage :: (PackageFixedDeps ipkg, PackageFixedDeps srcpkg,
                            HasUnitId ipkg, HasUnitId srcpkg)
                        => GenericInstallPlan ipkg srcpkg iresult ifailure
                        -> UnitId
                        -> GenericReadyPackage srcpkg
lookupProcessingPackage plan pkgid =
  case Graph.lookup pkgid (planIndex plan) of
    Just (Processing pkg) -> pkg
    _  -> internalError $ "not in processing state or no such pkg " ++
                          display pkgid

-- | Check a package that we expect to be in the configured or failed state.
--
checkConfiguredPackage :: (Package srcpkg, Package ipkg)
                       => GenericPlanPackage ipkg srcpkg iresult ifailure
                       -> Maybe srcpkg
checkConfiguredPackage (Configured pkg) = Just pkg
checkConfiguredPackage (Failed     _ _) = Nothing
checkConfiguredPackage pkg                =
  internalError $ "not configured or no such pkg " ++ display (packageId pkg)

-- | Replace a ready package with a pre-existing one. The pre-existing one
-- must have exactly the same dependencies as the source one was configured
-- with.
--
preexisting :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
                HasUnitId srcpkg, PackageFixedDeps srcpkg)
            => UnitId
            -> ipkg
            -> GenericInstallPlan ipkg srcpkg iresult ifailure
            -> GenericInstallPlan ipkg srcpkg iresult ifailure
preexisting pkgid ipkg plan = assert (invariant plan') plan'
  where
    plan' = plan {
      planIndex   = Graph.insert (PreExisting ipkg)
                    -- ...but be sure to use the *old* IPID for the lookup for
                    -- the preexisting record
                  . Graph.deleteKey pkgid
                  $ planIndex plan
    }

-- | Replace a ready package with an installed one. The installed one
-- must have exactly the same dependencies as the source one was configured
-- with.
--
preinstalled :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
                 HasUnitId srcpkg, PackageFixedDeps srcpkg)
             => UnitId
             -> Maybe ipkg -> iresult
             -> GenericInstallPlan ipkg srcpkg iresult ifailure
             -> GenericInstallPlan ipkg srcpkg iresult ifailure
preinstalled pkgid mipkg buildResult plan = assert (invariant plan') plan'
  where
    plan' = plan { planIndex = Graph.insert installed (planIndex plan) }
    Just installed = do
      Configured pkg <- Graph.lookup pkgid (planIndex plan)
      rpkg <- lookupReadyPackage plan pkg
      return (Installed rpkg mipkg buildResult)


-- ------------------------------------------------------------
-- * Checking validity of plans
-- ------------------------------------------------------------

-- | A valid installation plan is a set of packages that is 'acyclic',
-- 'closed' and 'consistent'. Also, every 'ConfiguredPackage' in the
-- plan has to have a valid configuration (see 'configuredPackageValid').
--
-- * if the result is @False@ use 'problems' to get a detailed list.
--
valid :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
          HasUnitId srcpkg, PackageFixedDeps srcpkg)
      => IndependentGoals
      -> PlanIndex ipkg srcpkg iresult ifailure
      -> Bool
valid indepGoals index =
    null $ problems indepGoals index

data PlanProblem ipkg srcpkg iresult ifailure =
     PackageMissingDeps   (GenericPlanPackage ipkg srcpkg iresult ifailure)
                          [PackageIdentifier]
   | PackageCycle         [GenericPlanPackage ipkg srcpkg iresult ifailure]
   | PackageStateInvalid  (GenericPlanPackage ipkg srcpkg iresult ifailure)
                          (GenericPlanPackage ipkg srcpkg iresult ifailure)

-- | For an invalid plan, produce a detailed list of problems as human readable
-- error messages. This is mainly intended for debugging purposes.
-- Use 'showPlanProblem' for a human readable explanation.
--
problems :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
             HasUnitId srcpkg, PackageFixedDeps srcpkg)
         => IndependentGoals
         -> PlanIndex ipkg srcpkg iresult ifailure
         -> [PlanProblem ipkg srcpkg iresult ifailure]
problems _indepGoals index =

     [ PackageMissingDeps pkg
       (catMaybes
        (map
         (fmap packageId . flip Graph.lookup index)
         missingDeps))
     | (pkg, missingDeps) <- Graph.broken index ]

  ++ [ PackageCycle cycleGroup
     | cycleGroup <- Graph.cycles index ]

  ++ [ PackageStateInvalid pkg pkg'
     | pkg <- Graph.toList index
     , Just pkg' <- map (flip Graph.lookup index)
                    (CD.flatDeps (depends pkg))
     , not (stateDependencyRelation pkg pkg') ]

-- | The states of packages have that depend on each other must respect
-- this relation. That is for very case where package @a@ depends on
-- package @b@ we require that @dependencyStatesOk a b = True@.
--
stateDependencyRelation :: GenericPlanPackage ipkg srcpkg iresult ifailure
                        -> GenericPlanPackage ipkg srcpkg iresult ifailure
                        -> Bool
stateDependencyRelation (PreExisting _) (PreExisting _)   = True

stateDependencyRelation (Configured  _) (PreExisting _)   = True
stateDependencyRelation (Configured  _) (Configured  _)   = True
stateDependencyRelation (Configured  _) (Processing  _)   = True
stateDependencyRelation (Configured  _) (Installed _ _ _) = True

stateDependencyRelation (Processing  _) (PreExisting _)   = True
stateDependencyRelation (Processing  _) (Installed _ _ _) = True

stateDependencyRelation (Installed _ _ _) (PreExisting _)   = True
stateDependencyRelation (Installed _ _ _) (Installed _ _ _) = True

stateDependencyRelation (Failed    _ _) (PreExisting _)   = True
-- failed can depends on configured because a package can depend on
-- several other packages and if one of the deps fail then we fail
-- but we still depend on the other ones that did not fail:
stateDependencyRelation (Failed    _ _) (Configured  _)   = True
stateDependencyRelation (Failed    _ _) (Processing  _)   = True
stateDependencyRelation (Failed    _ _) (Installed _ _ _) = True
stateDependencyRelation (Failed    _ _) (Failed    _   _) = True

stateDependencyRelation _               _                 = False




-- | Return all the packages in the 'InstallPlan' in reverse topological order.
-- That is, for each package, all depencencies of the package appear first.
--
-- Compared to 'executionOrder', this function returns all the installed and
-- source packages rather than just the source ones. Also, while both this
-- and 'executionOrder' produce reverse topological orderings of the package
-- dependency graph, it is not necessarily exactly the same order.
--
reverseTopologicalOrder :: GenericInstallPlan ipkg srcpkg iresult ifailure
                        -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
reverseTopologicalOrder plan = Graph.revTopSort (planIndex plan)


fromSolverInstallPlan ::
      (HasUnitId ipkg,   PackageFixedDeps ipkg,
       HasUnitId srcpkg, PackageFixedDeps srcpkg)
    -- Maybe this should be a UnitId not ConfiguredId?
    => (   (SolverId -> ConfiguredId)
        -> SolverInstallPlan.SolverPlanPackage
        -> GenericPlanPackage ipkg srcpkg iresult ifailure    )
    -> SolverInstallPlan
    -> GenericInstallPlan ipkg srcpkg iresult ifailure
fromSolverInstallPlan f plan =
    mkInstallPlan (Graph.fromList pkgs')
                  (SolverInstallPlan.planIndepGoals plan)
  where
    (_, pkgs') = foldl' f' (Map.empty, []) (SolverInstallPlan.reverseTopologicalOrder plan)

    f' (pidMap, pkgs) pkg = (pidMap', pkg' : pkgs)
      where
       pkg' = f (mapDep pidMap) pkg

       pidMap'
         = case sid of
            PreExistingId _pid uid ->
                assert (uid == uid') pidMap
            PlannedId pid ->
                Map.insert pid uid' pidMap
         where
           sid  = nodeKey pkg
           uid' = nodeKey pkg'

    mapDep _ (PreExistingId pid uid) = ConfiguredId pid uid
    mapDep pidMap (PlannedId pid)
        | Just uid <- Map.lookup pid pidMap
        = ConfiguredId pid uid
        -- This shouldn't happen, since mapDep should only be called
        -- on neighbor SolverId, which must have all been done already
        -- by the reverse top-sort (this also assumes that the graph
        -- is not broken).
        | otherwise
        = error ("fromSolverInstallPlan mapDep: " ++ display pid)

-- | Conversion of 'SolverInstallPlan' to 'InstallPlan'.
-- Similar to 'elaboratedInstallPlan'
configureInstallPlan :: SolverInstallPlan -> InstallPlan
configureInstallPlan solverPlan =
    flip fromSolverInstallPlan solverPlan $ \mapDep planpkg ->
      case planpkg of
        SolverInstallPlan.PreExisting pkg _ ->
          PreExisting pkg

        SolverInstallPlan.Configured  pkg ->
          Configured (configureSolverPackage mapDep pkg)
  where
    configureSolverPackage :: (SolverId -> ConfiguredId)
                           -> SolverPackage UnresolvedPkgLoc
                           -> ConfiguredPackage UnresolvedPkgLoc
    configureSolverPackage mapDep spkg =
      ConfiguredPackage {
        confPkgId = SimpleUnitId
                  $ Configure.computeComponentId
                        Cabal.NoFlag
                        (packageId spkg)
                        PD.CLibName
                        -- TODO: this is a hack that won't work for Backpack.
                        (map ((\(SimpleUnitId cid0) -> cid0) . confInstId)
                             (CD.libraryDeps deps))
                        (solverPkgFlags spkg),
        confPkgSource = solverPkgSource spkg,
        confPkgFlags  = solverPkgFlags spkg,
        confPkgStanzas = solverPkgStanzas spkg,
        confPkgDeps   = deps
      }
      where
        deps = fmap (map mapDep) (solverPkgDeps spkg)


-- ------------------------------------------------------------
-- * Primitives for traversing plans
-- ------------------------------------------------------------

-- $traversal
--
-- Algorithms to traverse or execute an 'InstallPlan', especially in parallel,
-- may make use of the 'Processing' type and the associated operations
-- 'ready', 'completed' and 'failed'.
--
-- The 'Processing' type is used to keep track of the state of a traversal and
-- includes the set of packages that are in the processing state, e.g. in the
-- process of being installed, plus those that have been completed and those
-- where processing failed.
--
-- Traversal algorithms start with an 'InstallPlan':
--
-- * Initially there will be certain packages that can be processed immediately
--   (since they are configured source packages and have all their dependencies
--   installed already). The function 'ready' returns these packages plus a
--   'Processing' state that marks these same packages as being in the
--   processing state.
--
-- * The algorithm must now arrange for these packages to be processed
--   (possibly in parallel). When a package has completed processing, the
--   algorithm needs to know which other packages (if any) are now ready to
--   process as a result. The 'completed' function marks a package as completed
--   and returns any packages that are newly in the processing state (ie ready
--   to process), along with the updated 'Processing' state.
--
-- * If failure is possible then when processing a package fails, the algorithm
--   needs to know which other packages have also failed as a result. The
--   'failed' function marks the given package as failed as well as all the
--   other packages that depend on the failed package. In addition it returns
--   the other failed packages.


-- | The 'Processing' type is used to keep track of the state of a traversal
-- and includes the set of packages that are in the processing state, e.g. in
-- the process of being installed, plus those that have been completed and
-- those where processing failed.
--
data Processing = Processing' !(Set UnitId) !(Set UnitId) !(Set UnitId)
                            -- processing,   completed,    failed

-- | The packages in the plan that are initially ready to be installed.
-- That is they are in the configured state and have all their dependencies
-- installed already.
--
-- The result is both the packages that are now ready to be installed and also
-- a 'Processing' state containing those same packages. The assumption is that
-- all the packages that are ready will now be processed and so we can consider
-- them to be in the processing state.
--
ready' :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
           HasUnitId srcpkg, PackageFixedDeps srcpkg)
      => GenericInstallPlan ipkg srcpkg unused1 unused2
      -> ([GenericReadyPackage srcpkg], Processing)
ready' plan =
    assert (processingInvariant plan processing) $
    (readyPackages, processing)
  where
    !processing =
      Processing'
        (Set.fromList [ installedUnitId pkg | pkg <- readyPackages ])
        (Set.fromList [ installedUnitId pkg | PreExisting pkg <- toList plan ])
        Set.empty
    readyPackages =
      [ ReadyPackage pkg
      | Configured pkg <- toList plan
      , all isPreExisting (directDeps plan (installedUnitId pkg))
      ]

    isPreExisting (PreExisting {}) = True
    isPreExisting _                = False


-- | Given a package in the processing state, mark the package as completed
-- and return any packages that are newly in the processing state (ie ready to
-- process), along with the updated 'Processing' state.
--
completed' :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
               HasUnitId srcpkg, PackageFixedDeps srcpkg)
          => GenericInstallPlan ipkg srcpkg unused1 unused2
          -> Processing -> UnitId
          -> ([GenericReadyPackage srcpkg], Processing)
completed' plan (Processing' processingSet completedSet failedSet) pkgid =
    assert (pkgid `Set.member` processingSet) $
    assert (processingInvariant plan processing') $

    ( map asReadyPackage newlyReady
    , processing' )
  where
    completedSet'  = Set.insert pkgid completedSet

    -- each direct reverse dep where all direct deps are completed
    newlyReady     = [ dep
                     | dep <- revDirectDeps plan pkgid
                     , all ((`Set.member` completedSet') . installedUnitId)
                           (directDeps plan (installedUnitId dep))
                     ]

    processingSet' = foldl' (flip Set.insert)
                            (Set.delete pkgid processingSet)
                            (map installedUnitId newlyReady)
    processing'    = Processing' processingSet' completedSet' failedSet

    asReadyPackage (Configured pkg) = ReadyPackage pkg
    asReadyPackage _ = error "InstallPlan.completed: internal error"

failed' :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
            HasUnitId srcpkg, PackageFixedDeps srcpkg)
       => GenericInstallPlan ipkg srcpkg unused1 unused2
       -> Processing -> UnitId
       -> ([srcpkg], Processing)
failed' plan (Processing' processingSet completedSet failedSet) pkgid =
    assert (pkgid `Set.member` processingSet) $
    assert (all (`Set.notMember` processingSet) (tail newlyFailedIds)) $
    assert (all (`Set.notMember` completedSet)  (tail newlyFailedIds)) $
    assert (all (`Set.notMember` failedSet)     (tail newlyFailedIds)) $
    assert (processingInvariant plan processing') $

    ( map asConfiguredPackage (tail newlyFailed)
    , processing' )
  where
    processingSet' = Set.delete pkgid processingSet
    failedSet'     = failedSet `Set.union` Set.fromList newlyFailedIds
    newlyFailedIds = map installedUnitId newlyFailed
    newlyFailed    = fromMaybe (internalError "package not in graph")
                   $ Graph.revClosure (planIndex plan) [pkgid]
    processing'    = Processing' processingSet' completedSet failedSet'

    asConfiguredPackage (Configured pkg) = pkg
    asConfiguredPackage _ = internalError "not in configured state"

directDeps, revDirectDeps
  :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
      HasUnitId srcpkg, PackageFixedDeps srcpkg)
  => GenericInstallPlan ipkg srcpkg iresult ifailure
  -> UnitId
  -> [GenericPlanPackage ipkg srcpkg iresult ifailure]

directDeps plan pkgid =
  case Graph.neighbors (planIndex plan) pkgid of
    Just deps -> deps
    Nothing   -> internalError "directDeps: package not in graph"

revDirectDeps plan pkgid =
  case Graph.revNeighbors (planIndex plan) pkgid of
    Just deps -> deps
    Nothing   -> internalError "directDeps: package not in graph"

processingInvariant :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
                        HasUnitId srcpkg, PackageFixedDeps srcpkg)
                    => GenericInstallPlan ipkg srcpkg unused1 unused2
                    -> Processing -> Bool
processingInvariant plan (Processing' processingSet completedSet failedSet) =
    all (isJust . flip Graph.lookup (planIndex plan)) (Set.toList processingSet)
 && all (isJust . flip Graph.lookup (planIndex plan)) (Set.toList completedSet)
 && all (isJust . flip Graph.lookup (planIndex plan)) (Set.toList failedSet)
 && noIntersection processingSet completedSet
 && noIntersection processingSet failedSet
 && noIntersection failedSet     completedSet
 && noIntersection processingClosure completedSet
 && noIntersection processingClosure failedSet
 && and [ case Graph.lookup pkgid (planIndex plan) of
            Just (Configured  _) -> True
            Just (PreExisting _) -> False
            _                    -> False
        | pkgid <- Set.toList processingSet ++ Set.toList failedSet ]
  where
    processingClosure = Set.fromList
                      . map installedUnitId
                      . fromMaybe (internalError "processingClosure")
                      . Graph.revClosure (planIndex plan)
                      . Set.toList
                      $ processingSet
    noIntersection a b = Set.null (Set.intersection a b)


-- ------------------------------------------------------------
-- * Traversing plans
-- ------------------------------------------------------------

-- | Flatten an 'InstallPlan', producing the sequence of source packages in
-- the order in which they would be processed when the plan is executed. This
-- can be used for simultations or presenting execution dry-runs.
--
-- It is guaranteed to give the same order as using 'execute' (with a serial
-- in-order 'JobControl'), which is a reverse topological orderings of the
-- source packages in the dependency graph, albeit not necessarily exactly the
-- same ordering as that produced by 'reverseTopologicalOrder'.
--
executionOrder :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
                   HasUnitId srcpkg, PackageFixedDeps srcpkg)
        => GenericInstallPlan ipkg srcpkg unused1 unused2
        -> [GenericReadyPackage srcpkg]
executionOrder plan =
    let (newpkgs, processing) = ready' plan
     in tryNewTasks processing newpkgs
  where
    tryNewTasks _processing []       = []
    tryNewTasks  processing (p:todo) = waitForTasks processing p todo

    waitForTasks processing p todo =
        p : tryNewTasks processing' (todo++nextpkgs)
      where
        (nextpkgs, processing') = completed' plan processing (installedUnitId p)


-- ------------------------------------------------------------
-- * Executing plans
-- ------------------------------------------------------------

-- | The set of results we get from executing an install plan.
--
type BuildResults failure result = Map UnitId (Either failure result)

-- | Lookup the build result for a single package.
--
lookupBuildResult :: HasUnitId pkg
                  => pkg -> BuildResults failure result
                  -> Maybe (Either failure result)
lookupBuildResult = Map.lookup . installedUnitId

-- | Execute an install plan. This traverses the plan in dependency order.
--
-- Executing each individual package can fail and if so all dependents fail
-- too. The result for each package is collected as a 'BuildResults' map.
--
-- Visiting each package happens with optional parallelism, as determined by
-- the 'JobControl'. By default, after any failure we stop as soon as possible
-- (using the 'JobControl' to try to cancel in-progress tasks). This behaviour
-- can be reversed to keep going and build as many packages as possible.
--
execute :: forall m ipkg srcpkg result failure unused1 unused2.
           (HasUnitId ipkg,   PackageFixedDeps ipkg,
            HasUnitId srcpkg, PackageFixedDeps srcpkg,
            Monad m)
        => JobControl m (UnitId, Either failure result)
        -> Bool                -- ^ Keep going after failure
        -> (srcpkg -> failure) -- ^ Value for dependents of failed packages
        -> GenericInstallPlan ipkg srcpkg unused1 unused2
        -> (GenericReadyPackage srcpkg -> m (Either failure result))
        -> m (BuildResults failure result)
execute jobCtl keepGoing depFailure plan installPkg =
    let (newpkgs, processing) = ready' plan
     in tryNewTasks Map.empty False False processing newpkgs
  where
    tryNewTasks :: BuildResults failure result
                -> Bool -> Bool -> Processing
                -> [GenericReadyPackage srcpkg]
                -> m (BuildResults failure result)

    tryNewTasks !results tasksFailed tasksRemaining !processing newpkgs
      -- we were in the process of cancelling and now we're finished
      | tasksFailed && not keepGoing && not tasksRemaining
      = return results

      -- we are still in the process of cancelling, wait for remaining tasks
      | tasksFailed && not keepGoing && tasksRemaining
      = waitForTasks results tasksFailed processing

      -- no new tasks to do and all tasks are done so we're finished
      | null newpkgs && not tasksRemaining
      = return results

      -- no new tasks to do, remaining tasks to wait for
      | null newpkgs
      = waitForTasks results tasksFailed processing

      -- new tasks to do, spawn them, then wait for tasks to complete
      | otherwise
      = do sequence_ [ spawnJob jobCtl $ do
                         result <- installPkg pkg
                         return (installedUnitId pkg, result)
                     | pkg <- newpkgs ]
           waitForTasks results tasksFailed processing

    waitForTasks :: BuildResults failure result
                 -> Bool -> Processing
                 -> m (BuildResults failure result)
    waitForTasks !results tasksFailed !processing = do
      (pkgid, result) <- collectJob jobCtl

      case result of

        Right _success -> do
            tasksRemaining <- remainingJobs jobCtl
            tryNewTasks results' tasksFailed tasksRemaining
                        processing' nextpkgs
          where
            results' = Map.insert pkgid result results
            (nextpkgs, processing') = completed' plan processing pkgid

        Left _failure -> do
            -- if this is the first failure and we're not trying to keep going
            -- then try to cancel as many of the remaining jobs as possible
            when (not tasksFailed && not keepGoing) $
              cancelJobs jobCtl

            tasksRemaining <- remainingJobs jobCtl
            tryNewTasks results' True tasksRemaining processing' []
          where
            (depsfailed, processing') = failed' plan processing pkgid
            results'   = Map.insert pkgid result results `Map.union` depResults
            depResults = Map.fromList
                           [ (installedUnitId deppkg, Left (depFailure deppkg))
                           | deppkg <- depsfailed ]
