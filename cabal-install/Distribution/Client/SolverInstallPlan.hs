{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.SolverInstallPlan
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'SolverInstallPlan' is the graph of packages produced by the
-- dependency solver, and specifies at the package-granularity what
-- things are going to be installed.  To put it another way: the
-- dependency solver produces a 'SolverInstallPlan', which is then
-- consumed by various other parts of Cabal.
--
-----------------------------------------------------------------------------
module Distribution.Client.SolverInstallPlan(
  SolverInstallPlan(..),
  SolverPlanPackage(..),

  -- * Operations on 'SolverInstallPlan's
  new,
  toList,

  remove,

  showPlanIndex,
  showInstallPlan,

  -- * Checking validity of plans
  valid,
  closed,
  consistent,
  acyclic,

  -- ** Details on invalid plans
  SolverPlanProblem(..),
  showPlanProblem,
  problems,

  -- ** Querying the install plan
  dependencyClosure,
  reverseDependencyClosure,
  topologicalOrder,
  reverseTopologicalOrder,
) where

import Distribution.Solver.Types.SolverPackage
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( PackageIdentifier(..), Package(..), PackageName(..)
         , HasUnitId(..), UnitId(..) )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Text
         ( display )

import Distribution.Client.Types
         ( UnresolvedPkgLoc )
import Distribution.Version
         ( Version )
import Distribution.Simple.PackageIndex
         ( PackageIndex )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Client.PlanIndex as PlanIndex

import           Distribution.Solver.Types.PackageFixedDeps
import           Distribution.Solver.Types.Settings

import Data.List
         ( intercalate )
import Data.Maybe
         ( fromMaybe, catMaybes )
import qualified Data.Graph as Graph
import Data.Graph (Graph)
import qualified Data.Tree as Tree
import Distribution.Compat.Binary (Binary(..))
import GHC.Generics

-- | The dependency solver produces two types of packages: pre-existing
-- packages that it selected from the installed package database, and
-- "configured" packages which need to be installed.
data SolverPlanPackage
   = PreExisting InstalledPackageInfo
   | Configured  (SolverPackage UnresolvedPkgLoc)
  deriving (Eq, Show, Generic)

instance Binary SolverPlanPackage

instance Package SolverPlanPackage where
  packageId (PreExisting ipkg)     = packageId ipkg
  packageId (Configured  spkg)     = packageId spkg

instance PackageFixedDeps SolverPlanPackage where
  depends (PreExisting pkg)     = depends pkg
  depends (Configured  pkg)     = depends pkg

instance HasUnitId SolverPlanPackage where
  installedUnitId (PreExisting ipkg ) = installedUnitId ipkg
  installedUnitId (Configured  spkg)  = installedUnitId spkg

type SolverPlanIndex = PackageIndex SolverPlanPackage

data SolverInstallPlan = SolverInstallPlan {
    planIndex      :: !SolverPlanIndex,
    planIndepGoals :: !IndependentGoals,

    -- | Cached (lazily) graph
    --
    -- The 'Graph' representation works in terms of integer node ids, so we
    -- have to keep mapping to and from our meaningful nodes, which of course
    -- are package ids.
    --
    planGraph      :: Graph,
    planGraphRev   :: Graph,  -- ^ Reverse deps, transposed
    planPkgIdOf    :: Graph.Vertex -> UnitId, -- ^ mapping back to package ids
    planVertexOf   :: UnitId -> Graph.Vertex  -- ^ mapping into node ids
  }

-- | Much like 'planPkgIdOf', but mapping back to full packages.
planPkgOf :: SolverInstallPlan
          -> Graph.Vertex
          -> SolverPlanPackage
planPkgOf plan v =
    case PackageIndex.lookupUnitId (planIndex plan)
                                   (planPkgIdOf plan v) of
      Just pkg -> pkg
      Nothing  -> error "InstallPlan: internal error: planPkgOf lookup failed"

-- | Smart constructor that deals with caching the 'Graph' representation.
--
mkInstallPlan :: SolverPlanIndex
              -> IndependentGoals
              -> SolverInstallPlan
mkInstallPlan index indepGoals =
    SolverInstallPlan {
      planIndex      = index,
      planIndepGoals = indepGoals,

      -- lazily cache the graph stuff:
      planGraph      = graph,
      planGraphRev   = Graph.transposeG graph,
      planPkgIdOf    = vertexToPkgId,
      planVertexOf   = fromMaybe noSuchPkgId . pkgIdToVertex
    }
  where
    (graph, vertexToPkgId, pkgIdToVertex) =
      PlanIndex.dependencyGraph index
    noSuchPkgId = internalError "package is not in the graph"

internalError :: String -> a
internalError msg = error $ "SolverInstallPlan: internal error: " ++ msg

instance Binary SolverInstallPlan where
    put SolverInstallPlan {
              planIndex      = index,
              planIndepGoals = indepGoals
        } = put (index, indepGoals)

    get = do
      (index, indepGoals) <- get
      return $! mkInstallPlan index indepGoals

showPlanIndex :: SolverPlanIndex -> String
showPlanIndex index =
    intercalate "\n" (map showPlanPackage (PackageIndex.allPackages index))
  where showPlanPackage p =
            showPlanPackageTag p ++ " "
                ++ display (packageId p) ++ " ("
                ++ display (installedUnitId p) ++ ")"

showInstallPlan :: SolverInstallPlan -> String
showInstallPlan = showPlanIndex . planIndex

showPlanPackageTag :: SolverPlanPackage -> String
showPlanPackageTag (PreExisting _)   = "PreExisting"
showPlanPackageTag (Configured  _)   = "Configured"

-- | Build an installation plan from a valid set of resolved packages.
--
new :: IndependentGoals
    -> SolverPlanIndex
    -> Either [SolverPlanProblem] SolverInstallPlan
new indepGoals index =
  case problems indepGoals index of
    []    -> Right (mkInstallPlan index indepGoals)
    probs -> Left probs

toList :: SolverInstallPlan -> [SolverPlanPackage]
toList = PackageIndex.allPackages . planIndex

-- | Remove packages from the install plan. This will result in an
-- error if there are remaining packages that depend on any matching
-- package. This is primarily useful for obtaining an install plan for
-- the dependencies of a package or set of packages without actually
-- installing the package itself, as when doing development.
--
remove :: (SolverPlanPackage -> Bool)
       -> SolverInstallPlan
       -> Either [SolverPlanProblem]
                 (SolverInstallPlan)
remove shouldRemove plan =
    new (planIndepGoals plan) newIndex
  where
    newIndex = PackageIndex.fromList $
                 filter (not . shouldRemove) (toList plan)

-- ------------------------------------------------------------
-- * Checking validity of plans
-- ------------------------------------------------------------

-- | A valid installation plan is a set of packages that is 'acyclic',
-- 'closed' and 'consistent'. Also, every 'ConfiguredPackage' in the
-- plan has to have a valid configuration (see 'configuredPackageValid').
--
-- * if the result is @False@ use 'problems' to get a detailed list.
--
valid :: IndependentGoals
      -> SolverPlanIndex
      -> Bool
valid indepGoals index =
    null $ problems indepGoals index

data SolverPlanProblem =
     PackageMissingDeps   SolverPlanPackage
                          [PackageIdentifier]
   | PackageCycle         [SolverPlanPackage]
   | PackageInconsistency PackageName [(PackageIdentifier, Version)]
   | PackageStateInvalid  SolverPlanPackage SolverPlanPackage

showPlanProblem :: SolverPlanProblem -> String
showPlanProblem (PackageMissingDeps pkg missingDeps) =
     "Package " ++ display (packageId pkg)
  ++ " depends on the following packages which are missing from the plan: "
  ++ intercalate ", " (map display missingDeps)

showPlanProblem (PackageCycle cycleGroup) =
     "The following packages are involved in a dependency cycle "
  ++ intercalate ", " (map (display.packageId) cycleGroup)

showPlanProblem (PackageInconsistency name inconsistencies) =
     "Package " ++ display name
  ++ " is required by several packages,"
  ++ " but they require inconsistent versions:\n"
  ++ unlines [ "  package " ++ display pkg ++ " requires "
                            ++ display (PackageIdentifier name ver)
             | (pkg, ver) <- inconsistencies ]

showPlanProblem (PackageStateInvalid pkg pkg') =
     "Package " ++ display (packageId pkg)
  ++ " is in the " ++ showPlanState pkg
  ++ " state but it depends on package " ++ display (packageId pkg')
  ++ " which is in the " ++ showPlanState pkg'
  ++ " state"
  where
    showPlanState (PreExisting _)   = "pre-existing"
    showPlanState (Configured  _)   = "configured"

-- | For an invalid plan, produce a detailed list of problems as human readable
-- error messages. This is mainly intended for debugging purposes.
-- Use 'showPlanProblem' for a human readable explanation.
--
problems :: IndependentGoals
         -> SolverPlanIndex
         -> [SolverPlanProblem]
problems indepGoals index =

     [ PackageMissingDeps pkg
       (catMaybes
        (map
         (fmap packageId . PackageIndex.lookupUnitId index)
         missingDeps))
     | (pkg, missingDeps) <- PlanIndex.brokenPackages index ]

  ++ [ PackageCycle cycleGroup
     | cycleGroup <- PlanIndex.dependencyCycles index ]

  ++ [ PackageInconsistency name inconsistencies
     | (name, inconsistencies) <-
       PlanIndex.dependencyInconsistencies indepGoals index ]

  ++ [ PackageStateInvalid pkg pkg'
     | pkg <- PackageIndex.allPackages index
     , Just pkg' <- map (PackageIndex.lookupUnitId index)
                    (CD.flatDeps (depends pkg))
     , not (stateDependencyRelation pkg pkg') ]

-- | The graph of packages (nodes) and dependencies (edges) must be acyclic.
--
-- * if the result is @False@ use 'PackageIndex.dependencyCycles' to find out
--   which packages are involved in dependency cycles.
--
acyclic :: SolverPlanIndex -> Bool
acyclic = null . PlanIndex.dependencyCycles

-- | An installation plan is closed if for every package in the set, all of
-- its dependencies are also in the set. That is, the set is closed under the
-- dependency relation.
--
-- * if the result is @False@ use 'PackageIndex.brokenPackages' to find out
--   which packages depend on packages not in the index.
--
closed :: SolverPlanIndex -> Bool
closed = null . PlanIndex.brokenPackages

-- | An installation plan is consistent if all dependencies that target a
-- single package name, target the same version.
--
-- This is slightly subtle. It is not the same as requiring that there be at
-- most one version of any package in the set. It only requires that of
-- packages which have more than one other package depending on them. We could
-- actually make the condition even more precise and say that different
-- versions are OK so long as they are not both in the transitive closure of
-- any other package (or equivalently that their inverse closures do not
-- intersect). The point is we do not want to have any packages depending
-- directly or indirectly on two different versions of the same package. The
-- current definition is just a safe approximation of that.
--
-- * if the result is @False@ use 'PackageIndex.dependencyInconsistencies' to
--   find out which packages are.
--
consistent :: SolverPlanIndex -> Bool
consistent = null . PlanIndex.dependencyInconsistencies (IndependentGoals False)

-- | The states of packages have that depend on each other must respect
-- this relation. That is for very case where package @a@ depends on
-- package @b@ we require that @dependencyStatesOk a b = True@.
--
stateDependencyRelation :: SolverPlanPackage
                        -> SolverPlanPackage
                        -> Bool
stateDependencyRelation (PreExisting _) (PreExisting _)   = True

stateDependencyRelation (Configured  _) (PreExisting _)   = True
stateDependencyRelation (Configured  _) (Configured  _)   = True

stateDependencyRelation _               _                 = False


-- | Compute the dependency closure of a package in a install plan
--
dependencyClosure :: SolverInstallPlan
                  -> [UnitId]
                  -> [SolverPlanPackage]
dependencyClosure plan =
    map (planPkgOf plan)
  . concatMap Tree.flatten
  . Graph.dfs (planGraph plan)
  . map (planVertexOf plan)


reverseDependencyClosure :: SolverInstallPlan
                         -> [UnitId]
                         -> [SolverPlanPackage]
reverseDependencyClosure plan =
    map (planPkgOf plan)
  . concatMap Tree.flatten
  . Graph.dfs (planGraphRev plan)
  . map (planVertexOf plan)


topologicalOrder :: SolverInstallPlan
                 -> [SolverPlanPackage]
topologicalOrder plan =
    map (planPkgOf plan)
  . Graph.topSort
  $ planGraph plan


reverseTopologicalOrder :: SolverInstallPlan
                        -> [SolverPlanPackage]
reverseTopologicalOrder plan =
    map (planPkgOf plan)
  . Graph.topSort
  $ planGraphRev plan
