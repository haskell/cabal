{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
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
  SolverInstallPlan,
  GenericInstallPlan,
  PlanPackage,
  SolverPlanPackage,
  GenericPlanPackage(..),

  -- * Operations on 'InstallPlan's
  new,
  toList,
  mapPreservingGraph,
  configureInstallPlan,

  ready,
  processing,
  completed,
  failed,
  remove,
  preexisting,
  preinstalled,

  showPlanIndex,
  showInstallPlan,

  -- * Checking validity of plans
  valid,
  closed,
  consistent,
  acyclic,

  -- ** Details on invalid plans
  PlanProblem(..),
  showPlanProblem,
  problems,

  -- ** Querying the install plan
  dependencyClosure,
  reverseDependencyClosure,
  topologicalOrder,
  reverseTopologicalOrder,
  ) where

import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( PackageIdentifier(..), PackageName(..), Package(..)
         , HasUnitId(..), UnitId(..) )
import Distribution.Client.Types
         ( BuildSuccess, BuildFailure
         , ConfiguredPackage(..), ConfiguredId(..)
         , UnresolvedPkgLoc
         , GenericReadyPackage(..) )
import Distribution.Version
         ( Version )
import Distribution.Simple.PackageIndex
         ( PackageIndex )
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Client.PlanIndex as PlanIndex
import Distribution.Text
         ( display )

import           Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.PackageFixedDeps
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SolverPackage

-- TODO: Need this when we compute final UnitIds
-- import qualified Distribution.Simple.Configure as Configure

import Data.List
         ( foldl', intercalate )
import Data.Maybe
         ( fromMaybe, catMaybes )
import qualified Data.Graph as Graph
import Data.Graph (Graph)
import qualified Data.Tree as Tree
import Distribution.Compat.Binary (Binary(..))
import GHC.Generics
import Control.Exception
         ( assert )
import qualified Data.Map as Map
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

instance (Binary ipkg, Binary srcpkg, Binary  iresult, Binary  ifailure)
      => Binary (GenericPlanPackage ipkg srcpkg iresult ifailure)

type PlanPackage = GenericPlanPackage
                   InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)
                   BuildSuccess BuildFailure

type SolverPlanPackage = GenericPlanPackage
                         InstalledPackageInfo (SolverPackage UnresolvedPkgLoc)
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
planPkgOf :: GenericInstallPlan ipkg srcpkg iresult ifailure
          -> Graph.Vertex
          -> GenericPlanPackage ipkg srcpkg iresult ifailure
planPkgOf plan v =
    case PackageIndex.lookupUnitId (planIndex plan)
                                   (planPkgIdOf plan v) of
      Just pkg -> pkg
      Nothing  -> error "InstallPlan: internal error: planPkgOf lookup failed"

-- | 'GenericInstallPlan' that the solver produces.  We'll "run this" in
-- order to compute the 'UnitId's for everything we want to build.
type SolverInstallPlan = GenericInstallPlan
                         InstalledPackageInfo (SolverPackage UnresolvedPkgLoc)
                         -- Technically, these are not used here, but
                         -- setting the type this way makes it easier
                         -- to run some operations.
                         BuildSuccess BuildFailure

-- | Conversion of 'SolverInstallPlan' to 'InstallPlan'.
-- Similar to 'elaboratedInstallPlan'
configureInstallPlan :: SolverInstallPlan -> InstallPlan
configureInstallPlan solverPlan =
    flip mapPreservingGraph solverPlan $ \mapDep planpkg ->
      case planpkg of
        PreExisting pkg ->
          PreExisting pkg

        Configured  pkg ->
          Configured (configureSolverPackage mapDep pkg)

        _ -> error "configureInstallPlan: unexpected package state"
  where
    configureSolverPackage :: (UnitId -> UnitId)
                           -> SolverPackage UnresolvedPkgLoc
                           -> ConfiguredPackage UnresolvedPkgLoc
    configureSolverPackage mapDep spkg =
      ConfiguredPackage {
        confPkgId = SimpleUnitId
                  $ Configure.computeComponentId
                        Cabal.NoFlag
                        (packageId spkg)
                        (PD.CLibName (display (pkgName (packageId spkg))))
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
        deps = fmap (map (configureSolverId mapDep)) (solverPkgDeps spkg)

    configureSolverId mapDep sid =
      ConfiguredId {
        confSrcId  = packageId sid, -- accurate!
        confInstId = mapDep (installedUnitId sid)
      }

-- | 'GenericInstallPlan' specialised to most commonly used types.
type InstallPlan = GenericInstallPlan
                   InstalledPackageInfo (ConfiguredPackage UnresolvedPkgLoc)
                   BuildSuccess BuildFailure

type PlanIndex ipkg srcpkg iresult ifailure =
     PackageIndex (GenericPlanPackage ipkg srcpkg iresult ifailure)

invariant :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
              HasUnitId srcpkg, PackageFixedDeps srcpkg)
          => GenericInstallPlan ipkg srcpkg iresult ifailure -> Bool
invariant plan =
    valid (planIndepGoals plan)
          (planIndex plan)

-- | Smart constructor that deals with caching the 'Graph' representation.
--
mkInstallPlan :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
                  HasUnitId srcpkg, PackageFixedDeps srcpkg)
              => PlanIndex ipkg srcpkg iresult ifailure
              -> IndependentGoals
              -> GenericInstallPlan ipkg srcpkg iresult ifailure
mkInstallPlan index indepGoals =
    GenericInstallPlan {
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
    intercalate "\n" (map showPlanPackage (PackageIndex.allPackages index))
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
toList = PackageIndex.allPackages . planIndex

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
    newIndex = PackageIndex.fromList $
                 filter (not . shouldRemove) (toList plan)

-- | The packages that are ready to be installed. That is they are in the
-- configured state and have all their dependencies installed already.
-- The plan is complete if the result is @[]@.
--
ready :: forall ipkg srcpkg iresult ifailure. PackageFixedDeps srcpkg
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
                      PackageFixedDeps srcpkg
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
      case PackageIndex.lookupUnitId (planIndex plan) pkgid of
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
processing :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
               HasUnitId srcpkg, PackageFixedDeps srcpkg)
           => [GenericReadyPackage srcpkg]
           -> GenericInstallPlan ipkg srcpkg iresult ifailure
           -> GenericInstallPlan ipkg srcpkg iresult ifailure
processing pkgs plan = assert (invariant plan') plan'
  where
    plan' = plan {
              planIndex = PackageIndex.merge (planIndex plan) processingPkgs
            }
    processingPkgs = PackageIndex.fromList [Processing pkg | pkg <- pkgs]

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
                  planIndex = PackageIndex.insert installed
                            . PackageIndex.deleteUnitId pkgid
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
                 planIndex = PackageIndex.merge (planIndex plan) failures
               }
    ReadyPackage srcpkg = lookupProcessingPackage plan pkgid
    failures = PackageIndex.fromList
             $ Failed srcpkg buildResult
             : [ Failed pkg' buildResult'
               | Just pkg' <- map checkConfiguredPackage
                            $ packagesThatDependOn plan pkgid ]

-- | Lookup the reachable packages in the reverse dependency graph.
--
packagesThatDependOn :: GenericInstallPlan ipkg srcpkg iresult ifailure
                     -> UnitId
                     -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
packagesThatDependOn plan pkgid = map (planPkgOf plan)
                          . tail
                          . Graph.reachable (planGraphRev plan)
                          . planVertexOf plan
                          $ pkgid

-- | Lookup a package that we expect to be in the processing state.
--
lookupProcessingPackage :: GenericInstallPlan ipkg srcpkg iresult ifailure
                        -> UnitId
                        -> GenericReadyPackage srcpkg
lookupProcessingPackage plan pkgid =
  case PackageIndex.lookupUnitId (planIndex plan) pkgid of
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
      planIndex   = PackageIndex.insert (PreExisting ipkg)
                    -- ...but be sure to use the *old* IPID for the lookup for
                    -- the preexisting record
                  . PackageIndex.deleteUnitId pkgid
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
    plan' = plan { planIndex = PackageIndex.insert installed (planIndex plan) }
    Just installed = do
      Configured pkg <- PackageIndex.lookupUnitId (planIndex plan) pkgid
      rpkg <- lookupReadyPackage plan pkg
      return (Installed rpkg mipkg buildResult)

-- | Transform an install plan by mapping a function over all the packages in
-- the plan. It can consistently change the 'UnitId' of all the packages,
-- while preserving the same overall graph structure.
--
-- The mapping function has a few constraints on it for correct operation.
-- The mapping function /may/ change the 'UnitId' of the package, but it
-- /must/ also remap the 'UnitId's of its dependencies using ths supplied
-- remapping function. Apart from this consistent remapping it /may not/
-- change the structure of the dependencies.
--
mapPreservingGraph :: (HasUnitId ipkg,
                       HasUnitId srcpkg,
                       HasUnitId ipkg',   PackageFixedDeps ipkg',
                       HasUnitId srcpkg', PackageFixedDeps srcpkg')
                   => (  (UnitId -> UnitId)
                      -> GenericPlanPackage ipkg  srcpkg  iresult  ifailure
                      -> GenericPlanPackage ipkg' srcpkg' iresult' ifailure')
                   -> GenericInstallPlan ipkg  srcpkg  iresult  ifailure
                   -> GenericInstallPlan ipkg' srcpkg' iresult' ifailure'
mapPreservingGraph f plan =
    mkInstallPlan (PackageIndex.fromList pkgs')
                  (planIndepGoals plan)
  where
    -- The package mapping function may change the UnitId. So we
    -- walk over the packages in dependency order keeping track of these
    -- package id changes and use it to supply the correct set of package
    -- dependencies as an extra input to the package mapping function.

    (_, pkgs') = foldl' f' (Map.empty, []) (reverseTopologicalOrder plan)

    f' (ipkgidMap, pkgs) pkg = (ipkgidMap', pkg' : pkgs)
      where
       pkg' = f (mapDep ipkgidMap) pkg

       ipkgidMap'
         | ipkgid /= ipkgid' = Map.insert ipkgid ipkgid' ipkgidMap
         | otherwise         =                           ipkgidMap
         where
           ipkgid  = installedUnitId pkg
           ipkgid' = installedUnitId pkg'

    mapDep ipkgidMap ipkgid = Map.findWithDefault ipkgid ipkgid ipkgidMap


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
   | PackageInconsistency PackageName [(PackageIdentifier, Version)]
   | PackageStateInvalid  (GenericPlanPackage ipkg srcpkg iresult ifailure)
                          (GenericPlanPackage ipkg srcpkg iresult ifailure)

showPlanProblem :: (Package ipkg, Package srcpkg)
                => PlanProblem ipkg srcpkg iresult ifailure -> String
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
    showPlanState (Processing  _)   = "processing"
    showPlanState (Installed _ _ _) = "installed"
    showPlanState (Failed    _   _) = "failed"

-- | For an invalid plan, produce a detailed list of problems as human readable
-- error messages. This is mainly intended for debugging purposes.
-- Use 'showPlanProblem' for a human readable explanation.
--
problems :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
             HasUnitId srcpkg, PackageFixedDeps srcpkg)
         => IndependentGoals
         -> PlanIndex ipkg srcpkg iresult ifailure
         -> [PlanProblem ipkg srcpkg iresult ifailure]
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
acyclic :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
            HasUnitId srcpkg, PackageFixedDeps srcpkg)
        => PlanIndex ipkg srcpkg iresult ifailure -> Bool
acyclic = null . PlanIndex.dependencyCycles

-- | An installation plan is closed if for every package in the set, all of
-- its dependencies are also in the set. That is, the set is closed under the
-- dependency relation.
--
-- * if the result is @False@ use 'PackageIndex.brokenPackages' to find out
--   which packages depend on packages not in the index.
--
closed :: (PackageFixedDeps ipkg,
           PackageFixedDeps srcpkg)
       => PlanIndex ipkg srcpkg iresult ifailure -> Bool
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
consistent :: (HasUnitId ipkg,   PackageFixedDeps ipkg,
               HasUnitId srcpkg, PackageFixedDeps srcpkg)
           => PlanIndex ipkg srcpkg iresult ifailure -> Bool
consistent = null . PlanIndex.dependencyInconsistencies (IndependentGoals False)

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


-- | Compute the dependency closure of a package in a install plan
--
dependencyClosure :: GenericInstallPlan ipkg srcpkg iresult ifailure
                  -> [UnitId]
                  -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
dependencyClosure plan =
    map (planPkgOf plan)
  . concatMap Tree.flatten
  . Graph.dfs (planGraph plan)
  . map (planVertexOf plan)


reverseDependencyClosure :: GenericInstallPlan ipkg srcpkg iresult ifailure
                         -> [UnitId]
                         -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
reverseDependencyClosure plan =
    map (planPkgOf plan)
  . concatMap Tree.flatten
  . Graph.dfs (planGraphRev plan)
  . map (planVertexOf plan)


topologicalOrder :: GenericInstallPlan ipkg srcpkg iresult ifailure
                 -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
topologicalOrder plan =
    map (planPkgOf plan)
  . Graph.topSort
  $ planGraph plan


reverseTopologicalOrder :: GenericInstallPlan ipkg srcpkg iresult ifailure
                        -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
reverseTopologicalOrder plan =
    map (planPkgOf plan)
  . Graph.topSort
  $ planGraphRev plan
