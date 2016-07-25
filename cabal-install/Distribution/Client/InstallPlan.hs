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
  remove,
  preexisting,

  -- * Traversal
  executionOrder,
  execute,
  BuildResults,
  lookupBuildResult,
  -- ** Traversal helpers
  -- $traversal
  Processing,
  ready,
  completed,
  failed,

  -- * Display
  showPlanIndex,
  showInstallPlan,
  showPlanProblem,

  -- * Graph-like operations
  topologicalOrder,
  reverseTopologicalOrder,
  ) where

import Distribution.Client.Types hiding (BuildResults)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Simple.Setup as Cabal

import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( PackageIdentifier(..), Package(..)
         , HasUnitId(..), UnitId(..) )
import Distribution.Solver.Types.SolverPackage
import           Distribution.Client.JobControl
import Distribution.Text
         ( display )
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.PackageFixedDeps
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SolverId

-- TODO: Need this when we compute final UnitIds
-- import qualified Distribution.Simple.Configure as Configure

import Data.List
         ( foldl', intercalate )
import Data.Maybe
         ( catMaybes, fromMaybe )
import qualified Distribution.Compat.Graph as Graph
import Distribution.Compat.Graph (Graph, IsNode(..))
import qualified Data.Graph as OldGraph
import Data.Array ((!), inRange, bounds)
import Distribution.Compat.Binary (Binary(..))
import GHC.Generics
import Control.Monad
import Control.Exception
         ( assert )
import qualified Data.Tree as Tree
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.IntSet as IntSet
import           Data.IntSet (IntSet)


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
data GenericPlanPackage ipkg srcpkg
   = PreExisting ipkg
   | Configured  srcpkg
  deriving (Eq, Show, Generic)

instance (HasUnitId ipkg,   PackageFixedDeps ipkg,
          HasUnitId srcpkg, PackageFixedDeps srcpkg)
         => IsNode (GenericPlanPackage ipkg srcpkg) where
    type Key (GenericPlanPackage ipkg srcpkg) = UnitId -- TODO: change me
    nodeKey = installedUnitId
    nodeNeighbors = CD.flatDeps . depends

instance (Binary ipkg, Binary srcpkg)
      => Binary (GenericPlanPackage ipkg srcpkg)

type PlanPackage = GenericPlanPackage
                   InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)

instance (Package ipkg, Package srcpkg) =>
         Package (GenericPlanPackage ipkg srcpkg) where
  packageId (PreExisting ipkg)     = packageId ipkg
  packageId (Configured  spkg)     = packageId spkg

instance (PackageFixedDeps srcpkg,
          PackageFixedDeps ipkg) =>
         PackageFixedDeps (GenericPlanPackage ipkg srcpkg) where
  depends (PreExisting pkg)     = depends pkg
  depends (Configured  pkg)     = depends pkg

instance (HasUnitId ipkg, HasUnitId srcpkg) =>
         HasUnitId
         (GenericPlanPackage ipkg srcpkg) where
  installedUnitId (PreExisting ipkg ) = installedUnitId ipkg
  installedUnitId (Configured  spkg)  = installedUnitId spkg

data GenericInstallPlan ipkg srcpkg = GenericInstallPlan {
    planIndex      :: !(PlanIndex ipkg srcpkg),
    planIndepGoals :: !IndependentGoals
  }

-- | 'GenericInstallPlan' specialised to most commonly used types.
type InstallPlan = GenericInstallPlan
                   InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)

type PlanIndex ipkg srcpkg =
     Graph (GenericPlanPackage ipkg srcpkg)

invariant :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
              HasUnitId srcpkg, PackageFixedDeps srcpkg)
          => GenericInstallPlan ipkg srcpkg -> Bool
invariant plan =
    valid (planIndepGoals plan)
          (planIndex plan)

-- | Smart constructor that deals with caching the 'Graph' representation.
--
mkInstallPlan :: PlanIndex ipkg srcpkg
              -> IndependentGoals
              -> GenericInstallPlan ipkg srcpkg
mkInstallPlan index indepGoals =
    GenericInstallPlan {
      planIndex      = index,
      planIndepGoals = indepGoals
    }

instance (HasUnitId ipkg,   PackageFixedDeps ipkg,
          HasUnitId srcpkg, PackageFixedDeps srcpkg,
          Binary ipkg, Binary srcpkg)
       => Binary (GenericInstallPlan ipkg srcpkg) where
    put GenericInstallPlan {
              planIndex      = index,
              planIndepGoals = indepGoals
        } = put (index, indepGoals)

    get = do
      (index, indepGoals) <- get
      return $! mkInstallPlan index indepGoals

showPlanIndex :: (HasUnitId ipkg, HasUnitId srcpkg)
              => PlanIndex ipkg srcpkg -> String
showPlanIndex index =
    intercalate "\n" (map showPlanPackage (Graph.toList index))
  where showPlanPackage p =
            showPlanPackageTag p ++ " "
                ++ display (packageId p) ++ " ("
                ++ display (installedUnitId p) ++ ")"

showInstallPlan :: (HasUnitId ipkg, HasUnitId srcpkg)
                => GenericInstallPlan ipkg srcpkg -> String
showInstallPlan = showPlanIndex . planIndex

showPlanPackageTag :: GenericPlanPackage ipkg srcpkg -> String
showPlanPackageTag (PreExisting _)   = "PreExisting"
showPlanPackageTag (Configured  _)   = "Configured"

-- | Build an installation plan from a valid set of resolved packages.
--
new :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
        HasUnitId srcpkg, PackageFixedDeps srcpkg)
    => IndependentGoals
    -> PlanIndex ipkg srcpkg
    -> Either [PlanProblem ipkg srcpkg]
              (GenericInstallPlan ipkg srcpkg)
new indepGoals index =
  case problems indepGoals index of
    []    -> Right (mkInstallPlan index indepGoals)
    probs -> Left probs

toList :: GenericInstallPlan ipkg srcpkg
       -> [GenericPlanPackage ipkg srcpkg]
toList = Graph.toList . planIndex

-- | Remove packages from the install plan. This will result in an
-- error if there are remaining packages that depend on any matching
-- package. This is primarily useful for obtaining an install plan for
-- the dependencies of a package or set of packages without actually
-- installing the package itself, as when doing development.
--
remove :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
           HasUnitId srcpkg, PackageFixedDeps srcpkg)
       => (GenericPlanPackage ipkg srcpkg -> Bool)
       -> GenericInstallPlan ipkg srcpkg
       -> Either [PlanProblem ipkg srcpkg]
                 (GenericInstallPlan ipkg srcpkg)
remove shouldRemove plan =
    new (planIndepGoals plan) newIndex
  where
    newIndex = Graph.fromList $
                 filter (not . shouldRemove) (toList plan)

-- | Replace a ready package with a pre-existing one. The pre-existing one
-- must have exactly the same dependencies as the source one was configured
-- with.
--
preexisting :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
                HasUnitId srcpkg, PackageFixedDeps srcpkg)
            => UnitId
            -> ipkg
            -> GenericInstallPlan ipkg srcpkg
            -> GenericInstallPlan ipkg srcpkg
preexisting pkgid ipkg plan = assert (invariant plan') plan'
  where
    plan' = plan {
      planIndex   = Graph.insert (PreExisting ipkg)
                    -- ...but be sure to use the *old* IPID for the lookup for
                    -- the preexisting record
                  . Graph.deleteKey pkgid
                  $ planIndex plan
    }


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
      -> PlanIndex ipkg srcpkg
      -> Bool
valid indepGoals index =
    null $ problems indepGoals index

data PlanProblem ipkg srcpkg =
     PackageMissingDeps   (GenericPlanPackage ipkg srcpkg)
                          [PackageIdentifier]
   | PackageCycle         [GenericPlanPackage ipkg srcpkg]
   | PackageStateInvalid  (GenericPlanPackage ipkg srcpkg)
                          (GenericPlanPackage ipkg srcpkg)

-- TODO: duplicate with SolverInstallPlan
showPlanProblem :: (Package ipkg, Package srcpkg) => PlanProblem ipkg srcpkg -> String
showPlanProblem (PackageMissingDeps pkg missingDeps) =
     "Package " ++ display (packageId pkg)
  ++ " depends on the following packages which are missing from the plan: "
  ++ intercalate ", " (map display missingDeps)

showPlanProblem (PackageCycle cycleGroup) =
     "The following packages are involved in a dependency cycle "
  ++ intercalate ", " (map (display.packageId) cycleGroup)

showPlanProblem (PackageStateInvalid pkg pkg') =
     "Package " ++ display (packageId pkg)
  ++ " is in the " ++ showPlanState pkg
  ++ " state but it depends on package " ++ display (packageId pkg')
  ++ " which is in the " ++ showPlanState pkg'
  ++ " state"
  where
    showPlanState (PreExisting _) = "pre-existing"
    showPlanState (Configured  _)   = "configured"


-- | For an invalid plan, produce a detailed list of problems as human readable
-- error messages. This is mainly intended for debugging purposes.
-- Use 'showPlanProblem' for a human readable explanation.
--
problems :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
             HasUnitId srcpkg, PackageFixedDeps srcpkg)
         => IndependentGoals
         -> PlanIndex ipkg srcpkg
         -> [PlanProblem ipkg srcpkg]
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
stateDependencyRelation :: GenericPlanPackage ipkg srcpkg
                        -> GenericPlanPackage ipkg srcpkg
                        -> Bool
stateDependencyRelation (PreExisting _) (PreExisting _) = True
stateDependencyRelation (Configured  _) (PreExisting _) = True
stateDependencyRelation (Configured  _) (Configured  _) = True
stateDependencyRelation (PreExisting _) (Configured  _) = False



topologicalOrder :: GenericInstallPlan ipkg srcpkg
                        -> [GenericPlanPackage ipkg srcpkg]
topologicalOrder plan = Graph.topSort (planIndex plan)

reverseTopologicalOrder :: GenericInstallPlan ipkg srcpkg
                        -> [GenericPlanPackage ipkg srcpkg]
reverseTopologicalOrder plan = Graph.revTopSort (planIndex plan)


fromSolverInstallPlan ::
      (HasUnitId ipkg,   PackageFixedDeps ipkg,
       HasUnitId srcpkg, PackageFixedDeps srcpkg)
    -- Maybe this should be a UnitId not ConfiguredId?
    => (   (SolverId -> ConfiguredId)
        -> SolverInstallPlan.SolverPlanPackage
        -> GenericPlanPackage ipkg srcpkg    )
    -> SolverInstallPlan
    -> GenericInstallPlan ipkg srcpkg
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
data Processing = Processing !IntSet !IntSet !IntSet
                          -- processing, completed, failed

planVertexOf :: GenericInstallPlan ipkg srcpkg -> UnitId -> OldGraph.Vertex
planVertexOf plan uid = fromMaybe (error "planVertexOf")
                                  (Graph.toVertex (planIndex plan) uid)

planPkgOf :: GenericInstallPlan ipkg srcpkg -> OldGraph.Vertex -> GenericPlanPackage ipkg srcpkg
planPkgOf plan v = Graph.fromVertex (planIndex plan) v

planGraph :: GenericInstallPlan ipkg srcpkg -> OldGraph.Graph
planGraph plan = let (g, _, _) = Graph.toGraph (planIndex plan) in g

planGraphRev :: GenericInstallPlan ipkg srcpkg -> OldGraph.Graph
planGraphRev plan = let (g, _, _) = Graph.toRevGraph (planIndex plan) in g

-- | The packages in the plan that are initially ready to be installed.
-- That is they are in the configured state and have all their dependencies
-- installed already.
--
-- The result is both the packages that are now ready to be installed and also
-- a 'Processing' state containing those same packages. The assumption is that
-- all the packages that are ready will now be processed and so we can consider
-- them to be in the processing state.
--
ready :: (HasUnitId ipkg, HasUnitId srcpkg)
      => GenericInstallPlan ipkg srcpkg
      -> ([GenericReadyPackage srcpkg], Processing)
ready plan =
    assert (processingInvariant plan processing) $
    (readyPackages, processing)
  where
    !processing =
      Processing
        (IntSet.fromList
          [ planVertexOf plan (installedUnitId pkg)
          | pkg <- readyPackages ])
        (IntSet.fromList
          [ planVertexOf plan (installedUnitId pkg)
          | PreExisting pkg <- toList plan ])
        IntSet.empty
    readyPackages =
      [ ReadyPackage pkg
      | Configured pkg <- toList plan
      , hasAllInstalledDeps pkg
      ]

    hasAllInstalledDeps pkg =
      all isPreExisting [ planPkgOf plan depv
                        | let pkgv = planVertexOf plan (installedUnitId pkg)
                        , depv <- planGraph plan ! pkgv ]
    isPreExisting (PreExisting {}) = True
    isPreExisting _                = False

-- | Given a package in the processing state, mark the package as completed
-- and return any packages that are newly in the processing state (ie ready to
-- process), along with the updated 'Processing' state.
--
completed :: GenericInstallPlan ipkg srcpkg
          -> Processing -> UnitId
          -> ([GenericReadyPackage srcpkg], Processing)
completed plan (Processing processingSet completedSet failedSet) pkgid =
    assert (pkgv `IntSet.member` processingSet) $
    assert (processingInvariant plan processing') $

    ( map (asReadyPackage . planPkgOf plan) newlyReady
    , processing' )
  where
    pkgv           = planVertexOf plan pkgid
    completedSet'  = IntSet.insert pkgv completedSet

    -- each direct reverse dep where all direct deps are completed
    newlyReady     = [ depv
                     | depv <- planGraphRev plan ! pkgv
                     , all (`IntSet.member` completedSet')
                           (planGraph plan ! depv)
                     ]

    processingSet' = foldl' (flip IntSet.insert)
                            (IntSet.delete pkgv processingSet)
                            newlyReady
    processing'    = Processing processingSet' completedSet' failedSet

    asReadyPackage (Configured pkg) = ReadyPackage pkg
    asReadyPackage _ = error "InstallPlan.completed: internal error"

failed :: GenericInstallPlan ipkg srcpkg
       -> Processing -> UnitId
       -> ([srcpkg], Processing)
failed plan (Processing processingSet completedSet failedSet) pkgid =
    assert (pkgv `IntSet.member` processingSet) $
    assert (all (`IntSet.notMember` processingSet) (tail newlyFailed)) $
    assert (all (`IntSet.notMember` completedSet)  (tail newlyFailed)) $
    assert (all (`IntSet.notMember` failedSet)     (tail newlyFailed)) $
    assert (processingInvariant plan processing') $

    ( map (asConfiguredPackage . planPkgOf plan) (tail newlyFailed)
    , processing' )
  where
    pkgv           = planVertexOf plan pkgid
    processingSet' = IntSet.delete pkgv processingSet
    failedSet'     = failedSet `IntSet.union` IntSet.fromList newlyFailed
    newlyFailed    = OldGraph.reachable (planGraphRev plan) pkgv
    processing'    = Processing processingSet' completedSet failedSet'

    asConfiguredPackage (Configured pkg) = pkg
    asConfiguredPackage _ = error "InstallPlan.failed: internal error"

processingInvariant :: GenericInstallPlan ipkg srcpkg -> Processing -> Bool
processingInvariant plan (Processing processingSet completedSet failedSet) =
    all (inRange (bounds (planGraph plan))) (IntSet.toList processingSet)
 && all (inRange (bounds (planGraph plan))) (IntSet.toList completedSet)
 && all (inRange (bounds (planGraph plan))) (IntSet.toList failedSet)
 && noIntersection processingSet completedSet
 && noIntersection processingSet failedSet
 && noIntersection failedSet     completedSet
 && noIntersection processingClosure completedSet
 && noIntersection processingClosure failedSet
 && and [ case planPkgOf plan pkgv of
            Configured  _ -> True
            PreExisting _ -> False
        | pkgv <- IntSet.toList processingSet ++ IntSet.toList failedSet ]
  where
    processingClosure = revDepClosure processingSet
    revDepClosure :: IntSet -> IntSet
    revDepClosure = IntSet.fromList
                  . concatMap Tree.flatten
                  . OldGraph.dfs (planGraphRev plan)
                  . IntSet.toList
    noIntersection a b = IntSet.null (IntSet.intersection a b)


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
executionOrder :: (HasUnitId ipkg, HasUnitId srcpkg)
        => GenericInstallPlan ipkg srcpkg
        -> [GenericReadyPackage srcpkg]
executionOrder plan =
    let (newpkgs, processing) = ready plan
     in tryNewTasks processing newpkgs
  where
    tryNewTasks _processing []       = []
    tryNewTasks  processing (p:todo) = waitForTasks processing p todo

    waitForTasks processing p todo =
        p : tryNewTasks processing' (todo++nextpkgs)
      where
        (nextpkgs, processing') = completed plan processing (installedUnitId p)


-- ------------------------------------------------------------
-- * Executing plans
-- ------------------------------------------------------------

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
execute :: forall m ipkg srcpkg result failure.
           (HasUnitId ipkg, HasUnitId srcpkg,
            Monad m)
        => JobControl m (UnitId, Either failure result)
        -> Bool                -- ^ Keep going after failure
        -> (srcpkg -> failure) -- ^ Value for dependents of failed packages
        -> GenericInstallPlan ipkg srcpkg
        -> (GenericReadyPackage srcpkg -> m (Either failure result))
        -> m (BuildResults failure result)
execute jobCtl keepGoing depFailure plan installPkg =
    let (newpkgs, processing) = ready plan
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
            (nextpkgs, processing') = completed plan processing pkgid

        Left _failure -> do
            -- if this is the first failure and we're not trying to keep going
            -- then try to cancel as many of the remaining jobs as possible
            when (not tasksFailed && not keepGoing) $
              cancelJobs jobCtl

            tasksRemaining <- remainingJobs jobCtl
            tryNewTasks results' True tasksRemaining processing' []
          where
            (depsfailed, processing') = failed plan processing pkgid
            results'   = Map.insert pkgid result results `Map.union` depResults
            depResults = Map.fromList
                           [ (installedUnitId deppkg, Left (depFailure deppkg))
                           | deppkg <- depsfailed ]
