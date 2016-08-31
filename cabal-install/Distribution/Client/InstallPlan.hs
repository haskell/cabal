{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
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
  IsUnit,

  -- * Operations on 'InstallPlan's
  new,
  toList,
  planIndepGoals,
  depends,

  fromSolverInstallPlan,
  configureInstallPlan,
  remove,
  preexisting,
  lookup,
  directDeps,
  revDirectDeps,

  -- * Traversal
  executionOrder,
  execute,
  BuildOutcomes,
  lookupBuildOutcome,
  -- ** Traversal helpers
  -- $traversal
  Processing,
  ready,
  completed,
  failed,

  -- * Display
  showPlanIndex,
  showInstallPlan,

  -- * Graph-like operations
  reverseTopologicalOrder,
  reverseDependencyClosure,
  ) where

import Distribution.Client.Types hiding (BuildOutcomes)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Simple.Setup as Cabal

import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( Package(..)
         , HasUnitId(..), UnitId(..) )
import Distribution.Solver.Types.SolverPackage
import Distribution.Client.JobControl
import Distribution.Text
import Text.PrettyPrint
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SolverId
import           Distribution.Solver.Types.InstSolverPackage

-- TODO: Need this when we compute final UnitIds
-- import qualified Distribution.Simple.Configure as Configure

import Data.List
         ( foldl' )
import Data.Maybe
         ( fromMaybe, isJust )
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

import Prelude hiding (lookup)


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
-- Cabal rather than from cabal-install). Instead, see 'PackageInstalled'.
data GenericPlanPackage ipkg srcpkg
   = PreExisting ipkg
   | Configured  srcpkg
  deriving (Eq, Show, Generic)

type IsUnit a = (IsNode a, Key a ~ UnitId)

depends :: IsUnit a => a -> [UnitId]
depends = nodeNeighbors

-- NB: Expanded constraint synonym here to avoid undecidable
-- instance errors in GHC 7.8 and earlier.
instance (IsNode ipkg, IsNode srcpkg, Key ipkg ~ UnitId, Key srcpkg ~ UnitId)
         => IsNode (GenericPlanPackage ipkg srcpkg) where
    type Key (GenericPlanPackage ipkg srcpkg) = UnitId
    nodeKey (PreExisting ipkg) = nodeKey ipkg
    nodeKey (Configured spkg) = nodeKey spkg
    nodeNeighbors (PreExisting ipkg) = nodeNeighbors ipkg
    nodeNeighbors (Configured spkg) = nodeNeighbors spkg

instance (Binary ipkg, Binary srcpkg)
      => Binary (GenericPlanPackage ipkg srcpkg)

type PlanPackage = GenericPlanPackage
                   InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)

instance (Package ipkg, Package srcpkg) =>
         Package (GenericPlanPackage ipkg srcpkg) where
  packageId (PreExisting ipkg)     = packageId ipkg
  packageId (Configured  spkg)     = packageId spkg

instance (HasUnitId ipkg, HasUnitId srcpkg) =>
         HasUnitId
         (GenericPlanPackage ipkg srcpkg) where
  installedUnitId (PreExisting ipkg) = installedUnitId ipkg
  installedUnitId (Configured  spkg) = installedUnitId spkg

instance (HasConfiguredId ipkg, HasConfiguredId srcpkg) =>
          HasConfiguredId (GenericPlanPackage ipkg srcpkg) where
    configuredId (PreExisting ipkg) = configuredId ipkg
    configuredId (Configured pkg) = configuredId pkg

data GenericInstallPlan ipkg srcpkg = GenericInstallPlan {
    planIndex      :: !(PlanIndex ipkg srcpkg),
    planIndepGoals :: !IndependentGoals
  }

-- | 'GenericInstallPlan' specialised to most commonly used types.
type InstallPlan = GenericInstallPlan
                   InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)

type PlanIndex ipkg srcpkg =
     Graph (GenericPlanPackage ipkg srcpkg)

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

internalError :: String -> a
internalError msg = error $ "InstallPlan: internal error: " ++ msg

instance (IsNode ipkg, Key ipkg ~ UnitId, IsNode srcpkg, Key srcpkg ~ UnitId,
          Binary ipkg, Binary srcpkg)
       => Binary (GenericInstallPlan ipkg srcpkg) where
    put GenericInstallPlan {
              planIndex      = index,
              planIndepGoals = indepGoals
        } = put (index, indepGoals)

    get = do
      (index, indepGoals) <- get
      return $! mkInstallPlan index indepGoals

showPlanIndex :: (Package ipkg, Package srcpkg,
                  IsUnit ipkg, IsUnit srcpkg)
              => PlanIndex ipkg srcpkg -> String
showPlanIndex index = renderStyle defaultStyle $
    vcat (map dispPlanPackage (Graph.toList index))
  where dispPlanPackage p =
            hang (hsep [ text (showPlanPackageTag p)
                       , disp (packageId p)
                       , parens (disp (nodeKey p))]) 2
                 (vcat (map disp (nodeNeighbors p)))

showInstallPlan :: (Package ipkg, Package srcpkg,
                    IsUnit ipkg, IsUnit srcpkg)
                => GenericInstallPlan ipkg srcpkg -> String
showInstallPlan = showPlanIndex . planIndex

showPlanPackageTag :: GenericPlanPackage ipkg srcpkg -> String
showPlanPackageTag (PreExisting _)   = "PreExisting"
showPlanPackageTag (Configured  _)   = "Configured"

-- | Build an installation plan from a valid set of resolved packages.
--
new :: IndependentGoals
    -> PlanIndex ipkg srcpkg
    -> GenericInstallPlan ipkg srcpkg
new indepGoals index = mkInstallPlan index indepGoals

toList :: GenericInstallPlan ipkg srcpkg
       -> [GenericPlanPackage ipkg srcpkg]
toList = Graph.toList . planIndex

-- | Remove packages from the install plan. This will result in an
-- error if there are remaining packages that depend on any matching
-- package. This is primarily useful for obtaining an install plan for
-- the dependencies of a package or set of packages without actually
-- installing the package itself, as when doing development.
--
remove :: (IsUnit ipkg, IsUnit srcpkg)
       => (GenericPlanPackage ipkg srcpkg -> Bool)
       -> GenericInstallPlan ipkg srcpkg
       -> GenericInstallPlan ipkg srcpkg
remove shouldRemove plan =
    new (planIndepGoals plan) newIndex
  where
    newIndex = Graph.fromList $
                 filter (not . shouldRemove) (toList plan)

-- | Replace a ready package with a pre-existing one. The pre-existing one
-- must have exactly the same dependencies as the source one was configured
-- with.
--
preexisting :: (IsUnit ipkg,
                IsUnit srcpkg)
            => UnitId
            -> ipkg
            -> GenericInstallPlan ipkg srcpkg
            -> GenericInstallPlan ipkg srcpkg
preexisting pkgid ipkg plan = plan'
  where
    plan' = plan {
      planIndex   = Graph.insert (PreExisting ipkg)
                    -- ...but be sure to use the *old* IPID for the lookup for
                    -- the preexisting record
                  . Graph.deleteKey pkgid
                  $ planIndex plan
    }

-- | Lookup a package in the plan.
--
lookup :: (IsUnit ipkg, IsUnit srcpkg)
       => GenericInstallPlan ipkg srcpkg
       -> UnitId
       -> Maybe (GenericPlanPackage ipkg srcpkg)
lookup plan pkgid = Graph.lookup pkgid (planIndex plan)

-- | Find all the direct depencencies of the given package.
--
-- Note that the package must exist in the plan or it is an error.
--
directDeps :: GenericInstallPlan ipkg srcpkg
           -> UnitId
           -> [GenericPlanPackage ipkg srcpkg]
directDeps plan pkgid =
  case Graph.neighbors (planIndex plan) pkgid of
    Just deps -> deps
    Nothing   -> internalError "directDeps: package not in graph"

-- | Find all the direct reverse depencencies of the given package.
--
-- Note that the package must exist in the plan or it is an error.
--
revDirectDeps :: GenericInstallPlan ipkg srcpkg
              -> UnitId
              -> [GenericPlanPackage ipkg srcpkg]
revDirectDeps plan pkgid =
  case Graph.revNeighbors (planIndex plan) pkgid of
    Just deps -> deps
    Nothing   -> internalError "revDirectDeps: package not in graph"







-- | Return all the packages in the 'InstallPlan' in reverse topological order.
-- That is, for each package, all depencencies of the package appear first.
--
-- Compared to 'executionOrder', this function returns all the installed and
-- source packages rather than just the source ones. Also, while both this
-- and 'executionOrder' produce reverse topological orderings of the package
-- dependency graph, it is not necessarily exactly the same order.
--
reverseTopologicalOrder :: GenericInstallPlan ipkg srcpkg
                        -> [GenericPlanPackage ipkg srcpkg]
reverseTopologicalOrder plan = Graph.revTopSort (planIndex plan)


-- | Return the packages in the plan that depend directly or indirectly on the
-- given packages.
--
reverseDependencyClosure :: GenericInstallPlan ipkg srcpkg
                         -> [UnitId]
                         -> [GenericPlanPackage ipkg srcpkg]
reverseDependencyClosure plan = fromMaybe []
                              . Graph.revClosure (planIndex plan)


-- Alert alert!   Why does SolverId map to a LIST of plan packages?
-- The sordid story has to do with 'build-depends' on a package
-- with libraries and executables.  In an ideal world, we would
-- ONLY depend on the library in this situation.  But c.f. #3661
-- some people rely on the build-depends to ALSO implicitly
-- depend on an executable.
--
-- I don't want to commit to a strategy yet, so the only possible
-- thing you can do in this case is return EVERYTHING and let
-- the client filter out what they want (executables? libraries?
-- etc).  This similarly implies we can't return a 'ConfiguredId'
-- because that's not enough information.

fromSolverInstallPlan ::
      (IsUnit ipkg, IsUnit srcpkg)
    => (   (SolverId -> [GenericPlanPackage ipkg srcpkg])
        -> SolverInstallPlan.SolverPlanPackage
        -> [GenericPlanPackage ipkg srcpkg]         )
    -> SolverInstallPlan
    -> GenericInstallPlan ipkg srcpkg
fromSolverInstallPlan f plan =
    mkInstallPlan (Graph.fromList pkgs'')
                  (SolverInstallPlan.planIndepGoals plan)
  where
    (_, _, pkgs'') = foldl' f' (Map.empty, Map.empty, [])
                        (SolverInstallPlan.reverseTopologicalOrder plan)

    f' (pidMap, ipiMap, pkgs) pkg = (pidMap', ipiMap', pkgs' ++ pkgs)
      where
       pkgs' = f (mapDep pidMap ipiMap) pkg

       (pidMap', ipiMap')
         = case nodeKey pkg of
            PreExistingId _ uid -> (pidMap, Map.insert uid pkgs' ipiMap)
            PlannedId     pid   -> (Map.insert pid pkgs' pidMap, ipiMap)

    mapDep _ ipiMap (PreExistingId _pid uid)
        | Just pkgs <- Map.lookup uid ipiMap = pkgs
        | otherwise = error ("fromSolverInstallPlan: PreExistingId " ++ display uid)
    mapDep pidMap _ (PlannedId pid)
        | Just pkgs <- Map.lookup pid pidMap = pkgs
        | otherwise = error ("fromSolverInstallPlan: PlannedId " ++ display pid)
    -- This shouldn't happen, since mapDep should only be called
    -- on neighbor SolverId, which must have all been done already
    -- by the reverse top-sort (we assume the graph is not broken).

-- | Conversion of 'SolverInstallPlan' to 'InstallPlan'.
-- Similar to 'elaboratedInstallPlan'
configureInstallPlan :: SolverInstallPlan -> InstallPlan
configureInstallPlan solverPlan =
    flip fromSolverInstallPlan solverPlan $ \mapDep planpkg ->
      [case planpkg of
        SolverInstallPlan.PreExisting pkg ->
          PreExisting (instSolverPkgIPI pkg)

        SolverInstallPlan.Configured  pkg ->
          Configured (configureSolverPackage mapDep pkg)
      ]
  where
    configureSolverPackage :: (SolverId -> [PlanPackage])
                           -> SolverPackage UnresolvedPkgLoc
                           -> ConfiguredPackage UnresolvedPkgLoc
    configureSolverPackage mapDep spkg =
      ConfiguredPackage {
        confPkgId = Configure.computeComponentId
                        Cabal.NoFlag
                        Cabal.NoFlag
                        (packageId spkg)
                        PD.CLibName
                        (map confInstId (CD.libraryDeps deps))
                        (solverPkgFlags spkg),
        confPkgSource = solverPkgSource spkg,
        confPkgFlags  = solverPkgFlags spkg,
        confPkgStanzas = solverPkgStanzas spkg,
        confPkgDeps   = deps
        -- NB: no support for executable dependencies
      }
      where
        deps = fmap (concatMap (map configuredId . mapDep)) (solverPkgLibDeps spkg)


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
data Processing = Processing !(Set UnitId) !(Set UnitId) !(Set UnitId)
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
ready :: (IsUnit ipkg, IsUnit srcpkg)
      => GenericInstallPlan ipkg srcpkg
      -> ([GenericReadyPackage srcpkg], Processing)
ready plan =
    assert (processingInvariant plan processing) $
    (readyPackages, processing)
  where
    !processing =
      Processing
        (Set.fromList [ nodeKey pkg | pkg <- readyPackages ])
        (Set.fromList [ nodeKey pkg | PreExisting pkg <- toList plan ])
        Set.empty
    readyPackages =
      [ ReadyPackage pkg
      | Configured pkg <- toList plan
      , all isPreExisting (directDeps plan (nodeKey pkg))
      ]

    isPreExisting (PreExisting {}) = True
    isPreExisting _                = False


-- | Given a package in the processing state, mark the package as completed
-- and return any packages that are newly in the processing state (ie ready to
-- process), along with the updated 'Processing' state.
--
completed :: (IsUnit ipkg, IsUnit srcpkg)
          => GenericInstallPlan ipkg srcpkg
          -> Processing -> UnitId
          -> ([GenericReadyPackage srcpkg], Processing)
completed plan (Processing processingSet completedSet failedSet) pkgid =
    assert (pkgid `Set.member` processingSet) $
    assert (processingInvariant plan processing') $

    ( map asReadyPackage newlyReady
    , processing' )
  where
    completedSet'  = Set.insert pkgid completedSet

    -- each direct reverse dep where all direct deps are completed
    newlyReady     = [ dep
                     | dep <- revDirectDeps plan pkgid
                     , all ((`Set.member` completedSet') . nodeKey)
                           (directDeps plan (nodeKey dep))
                     ]

    processingSet' = foldl' (flip Set.insert)
                            (Set.delete pkgid processingSet)
                            (map nodeKey newlyReady)
    processing'    = Processing processingSet' completedSet' failedSet

    asReadyPackage (Configured pkg) = ReadyPackage pkg
    asReadyPackage _ = error "InstallPlan.completed: internal error"

failed :: (IsUnit ipkg, IsUnit srcpkg)
       => GenericInstallPlan ipkg srcpkg
       -> Processing -> UnitId
       -> ([srcpkg], Processing)
failed plan (Processing processingSet completedSet failedSet) pkgid =
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
    newlyFailedIds = map nodeKey newlyFailed
    newlyFailed    = fromMaybe (internalError "package not in graph")
                   $ Graph.revClosure (planIndex plan) [pkgid]
    processing'    = Processing processingSet' completedSet failedSet'

    asConfiguredPackage (Configured pkg) = pkg
    asConfiguredPackage _ = internalError "not in configured state"

processingInvariant :: (IsUnit ipkg, IsUnit srcpkg)
                    => GenericInstallPlan ipkg srcpkg
                    -> Processing -> Bool
processingInvariant plan (Processing processingSet completedSet failedSet) =
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
            Nothing              -> False 
        | pkgid <- Set.toList processingSet ++ Set.toList failedSet ]
  where
    processingClosure = Set.fromList
                      . map nodeKey
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
executionOrder :: (IsUnit ipkg, IsUnit srcpkg)
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
        (nextpkgs, processing') = completed plan processing (nodeKey p)


-- ------------------------------------------------------------
-- * Executing plans
-- ------------------------------------------------------------

-- | The set of results we get from executing an install plan.
--
type BuildOutcomes failure result = Map UnitId (Either failure result)

-- | Lookup the build result for a single package.
--
lookupBuildOutcome :: HasUnitId pkg
                   => pkg -> BuildOutcomes failure result
                   -> Maybe (Either failure result)
lookupBuildOutcome = Map.lookup . installedUnitId

-- | Execute an install plan. This traverses the plan in dependency order.
--
-- Executing each individual package can fail and if so all dependents fail
-- too. The result for each package is collected as a 'BuildOutcomes' map.
--
-- Visiting each package happens with optional parallelism, as determined by
-- the 'JobControl'. By default, after any failure we stop as soon as possible
-- (using the 'JobControl' to try to cancel in-progress tasks). This behaviour
-- can be reversed to keep going and build as many packages as possible.
--
execute :: forall m ipkg srcpkg result failure.
           (IsUnit ipkg, IsUnit srcpkg,
            Monad m)
        => JobControl m (UnitId, Either failure result)
        -> Bool                -- ^ Keep going after failure
        -> (srcpkg -> failure) -- ^ Value for dependents of failed packages
        -> GenericInstallPlan ipkg srcpkg
        -> (GenericReadyPackage srcpkg -> m (Either failure result))
        -> m (BuildOutcomes failure result)
execute jobCtl keepGoing depFailure plan installPkg =
    let (newpkgs, processing) = ready plan
     in tryNewTasks Map.empty False False processing newpkgs
  where
    tryNewTasks :: BuildOutcomes failure result
                -> Bool -> Bool -> Processing
                -> [GenericReadyPackage srcpkg]
                -> m (BuildOutcomes failure result)

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
                         return (nodeKey pkg, result)
                     | pkg <- newpkgs ]
           waitForTasks results tasksFailed processing

    waitForTasks :: BuildOutcomes failure result
                 -> Bool -> Processing
                 -> m (BuildOutcomes failure result)
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
                           [ (nodeKey deppkg, Left (depFailure deppkg))
                           | deppkg <- depsfailed ]
