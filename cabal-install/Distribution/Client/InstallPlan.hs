{-# LANGUAGE ScopedTypeVariables #-}
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
  ready,
  processing,
  completed,
  failed,
  remove,
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
  ) where

import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( PackageIdentifier(..), PackageName(..), Package(..)
         , InstalledPackageId, HasInstalledPackageId(..) )
import Distribution.Client.Types
         ( BuildSuccess, BuildFailure
         , PackageFixedDeps(..), ConfiguredPackage
         , GenericReadyPackage(..), fakeInstalledPackageId )
import Distribution.Version
         ( Version )
import Distribution.Client.ComponentDeps (ComponentDeps)
import qualified Distribution.Client.ComponentDeps as CD
import Distribution.Simple.PackageIndex
         ( PackageIndex )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Client.PlanIndex
         ( FakeMap )
import qualified Distribution.Client.PlanIndex as PlanIndex
import Distribution.Text
         ( display )

import Data.List
         ( intercalate )
import Data.Maybe
         ( fromMaybe, maybeToList )
import qualified Data.Graph as Graph
import Data.Graph (Graph)
import Control.Exception
         ( assert )
import Data.Maybe (catMaybes)
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
   | Processing  (GenericReadyPackage srcpkg ipkg)
   | Installed   (GenericReadyPackage srcpkg ipkg) (Maybe ipkg) iresult
   | Failed      srcpkg ifailure

type PlanPackage = GenericPlanPackage
                   InstalledPackageInfo ConfiguredPackage
                   BuildSuccess BuildFailure

instance (Package ipkg, Package srcpkg) =>
         Package (GenericPlanPackage ipkg srcpkg iresult ifailure) where
  packageId (PreExisting ipkg)     = packageId ipkg
  packageId (Configured  spkg)     = packageId spkg
  packageId (Processing  rpkg)     = packageId rpkg
  packageId (Installed   rpkg _ _) = packageId rpkg
  packageId (Failed      spkg   _) = packageId spkg

instance (PackageFixedDeps srcpkg,
          PackageFixedDeps ipkg, HasInstalledPackageId ipkg) =>
         PackageFixedDeps (GenericPlanPackage ipkg srcpkg iresult ifailure) where
  depends (PreExisting pkg)     = depends pkg
  depends (Configured  pkg)     = depends pkg
  depends (Processing  pkg)     = depends pkg
  depends (Installed   pkg _ _) = depends pkg
  depends (Failed      pkg   _) = depends pkg

instance (HasInstalledPackageId ipkg, HasInstalledPackageId srcpkg) =>
         HasInstalledPackageId
         (GenericPlanPackage ipkg srcpkg iresult ifailure) where
  installedPackageId (PreExisting ipkg ) = installedPackageId ipkg
  installedPackageId (Configured  spkg)  = installedPackageId spkg
  installedPackageId (Processing  rpkg)  = installedPackageId rpkg
  -- NB: defer to the actual installed package info in this case
  installedPackageId (Installed _ (Just ipkg) _) = installedPackageId ipkg
  installedPackageId (Installed rpkg _        _) = installedPackageId rpkg
  installedPackageId (Failed      spkg        _) = installedPackageId spkg


data GenericInstallPlan ipkg srcpkg iresult ifailure = GenericInstallPlan {
    planIndex      :: (PlanIndex ipkg srcpkg iresult ifailure),
    planFakeMap    :: FakeMap,
    planGraph      :: Graph,
    planGraphRev   :: Graph,
    planPkgOf      :: Graph.Vertex
                      -> GenericPlanPackage ipkg srcpkg iresult ifailure,
    planVertexOf   :: InstalledPackageId -> Graph.Vertex,
    planIndepGoals :: Bool
  }

-- | 'GenericInstallPlan' specialised to most commonly used types.
type InstallPlan = GenericInstallPlan
                   InstalledPackageInfo ConfiguredPackage
                   BuildSuccess BuildFailure

type PlanIndex ipkg srcpkg iresult ifailure =
     PackageIndex (GenericPlanPackage ipkg srcpkg iresult ifailure)

invariant :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
              HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
          => GenericInstallPlan ipkg srcpkg iresult ifailure -> Bool
invariant plan =
    valid (planFakeMap plan)
          (planIndepGoals plan)
          (planIndex plan)

internalError :: String -> a
internalError msg = error $ "InstallPlan: internal error: " ++ msg

showPlanIndex :: (HasInstalledPackageId ipkg, HasInstalledPackageId srcpkg)
              => PlanIndex ipkg srcpkg iresult ifailure -> String
showPlanIndex index =
    intercalate "\n" (map showPlanPackage (PackageIndex.allPackages index))
  where showPlanPackage p =
            showPlanPackageTag p ++ " "
                ++ display (packageId p) ++ " ("
                ++ display (installedPackageId p) ++ ")"

showInstallPlan :: (HasInstalledPackageId ipkg, HasInstalledPackageId srcpkg)
                => GenericInstallPlan ipkg srcpkg iresult ifailure -> String
showInstallPlan plan =
    showPlanIndex (planIndex plan) ++ "\n" ++
    "fake map:\n  " ++
    intercalate "\n  " (map showKV (Map.toList (planFakeMap plan)))
  where showKV (k,v) = display k ++ " -> " ++ display v

showPlanPackageTag :: GenericPlanPackage ipkg srcpkg iresult ifailure -> String
showPlanPackageTag (PreExisting _)   = "PreExisting"
showPlanPackageTag (Configured  _)   = "Configured"
showPlanPackageTag (Processing  _)   = "Processing"
showPlanPackageTag (Installed _ _ _) = "Installed"
showPlanPackageTag (Failed    _   _) = "Failed"

-- | Build an installation plan from a valid set of resolved packages.
--
new :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
        HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
    => Bool
    -> PlanIndex ipkg srcpkg iresult ifailure
    -> Either [PlanProblem ipkg srcpkg iresult ifailure]
              (GenericInstallPlan ipkg srcpkg iresult ifailure)
new indepGoals index =
  -- NB: Need to pre-initialize the fake-map with pre-existing
  -- packages
  let isPreExisting (PreExisting _) = True
      isPreExisting _ = False
      fakeMap = Map.fromList
              . map (\p -> (fakeInstalledPackageId (packageId p)
                           ,installedPackageId p))
              . filter isPreExisting
              $ PackageIndex.allPackages index in
  case problems fakeMap indepGoals index of
    [] -> Right GenericInstallPlan {
            planIndex      = index,
            planFakeMap    = fakeMap,
            planGraph      = graph,
            planGraphRev   = Graph.transposeG graph,
            planPkgOf      = vertexToPkgId,
            planVertexOf   = fromMaybe noSuchPkgId . pkgIdToVertex,
            planIndepGoals = indepGoals
          }
      where (graph, vertexToPkgId, pkgIdToVertex) =
              PlanIndex.dependencyGraph fakeMap index
            noSuchPkgId = internalError "package is not in the graph"
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
remove :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
           HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
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
      -> [GenericReadyPackage srcpkg ipkg]
ready plan = assert check readyPackages
  where
    check = if null readyPackages && null processingPackages
              then null configuredPackages
              else True
    configuredPackages = [ pkg | Configured pkg <- toList plan ]
    processingPackages = [ pkg | Processing pkg <- toList plan]

    readyPackages :: [GenericReadyPackage srcpkg ipkg]
    readyPackages =
      [ ReadyPackage srcpkg deps
      | srcpkg <- configuredPackages
        -- select only the package that have all of their deps installed:
      , deps <- maybeToList (hasAllInstalledDeps srcpkg)
      ]

    hasAllInstalledDeps :: srcpkg -> Maybe (ComponentDeps [ipkg])
    hasAllInstalledDeps = T.mapM (mapM isInstalledDep) . depends

    isInstalledDep :: InstalledPackageId -> Maybe ipkg
    isInstalledDep pkgid =
      -- NB: Need to check if the ID has been updated in planFakeMap, in which
      -- case we might be dealing with an old pointer
      case PlanIndex.fakeLookupInstalledPackageId
           (planFakeMap plan) (planIndex plan) pkgid
      of
        Just (PreExisting ipkg)            -> Just ipkg
        Just (Configured  _)               -> Nothing
        Just (Processing  _)               -> Nothing
        Just (Installed   _ (Just ipkg) _) -> Just ipkg
        Just (Installed   _ Nothing     _) -> internalError depOnNonLib
        Just (Failed      _             _) -> internalError depOnFailed
        Nothing                            -> internalError incomplete
    incomplete  = "install plan is not closed"
    depOnFailed = "configured package depends on failed package"
    depOnNonLib = "configured package depends on a non-library package"

-- | Marks packages in the graph as currently processing (e.g. building).
--
-- * The package must exist in the graph and be in the configured state.
--
processing :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
               HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
           => [GenericReadyPackage srcpkg ipkg]
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
completed :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
              HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
          => InstalledPackageId
          -> Maybe ipkg -> iresult
          -> GenericInstallPlan ipkg srcpkg iresult ifailure
          -> GenericInstallPlan ipkg srcpkg iresult ifailure
completed pkgid mipkg buildResult plan = assert (invariant plan') plan'
  where
    plan'     = plan {
                  -- NB: installation can change the IPID, so better
                  -- record it in the fake mapping...
                  planFakeMap = insert_fake_mapping mipkg
                              $ planFakeMap plan,
                  planIndex = PackageIndex.insert installed
                            . PackageIndex.deleteInstalledPackageId pkgid
                            $ planIndex plan
                }
    -- ...but be sure to use the *old* IPID for the lookup for the
    -- preexisting record
    installed = Installed (lookupProcessingPackage plan pkgid) mipkg buildResult
    insert_fake_mapping (Just ipkg) = Map.insert pkgid (installedPackageId ipkg)
    insert_fake_mapping  _          = id

-- | Marks a package in the graph as having failed. It also marks all the
-- packages that depended on it as having failed.
--
-- * The package must exist in the graph and be in the processing
-- state.
--
failed :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
           HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
       => InstalledPackageId -- ^ The id of the package that failed to install
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
    ReadyPackage srcpkg _deps = lookupProcessingPackage plan pkgid
    failures = PackageIndex.fromList
             $ Failed srcpkg buildResult
             : [ Failed pkg' buildResult'
               | Just pkg' <- map checkConfiguredPackage
                            $ packagesThatDependOn plan pkgid ]

-- | Lookup the reachable packages in the reverse dependency graph.
--
packagesThatDependOn :: GenericInstallPlan ipkg srcpkg iresult ifailure
                     -> InstalledPackageId
                     -> [GenericPlanPackage ipkg srcpkg iresult ifailure]
packagesThatDependOn plan pkgid = map (planPkgOf plan)
                          . tail
                          . Graph.reachable (planGraphRev plan)
                          . planVertexOf plan
                          $ Map.findWithDefault pkgid pkgid (planFakeMap plan)

-- | Lookup a package that we expect to be in the processing state.
--
lookupProcessingPackage :: GenericInstallPlan ipkg srcpkg iresult ifailure
                        -> InstalledPackageId
                        -> GenericReadyPackage srcpkg ipkg
lookupProcessingPackage plan pkgid =
  -- NB: processing packages are guaranteed to not indirect through
  -- planFakeMap
  case PackageIndex.lookupInstalledPackageId (planIndex plan) pkgid of
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

-- ------------------------------------------------------------
-- * Checking validity of plans
-- ------------------------------------------------------------

-- | A valid installation plan is a set of packages that is 'acyclic',
-- 'closed' and 'consistent'. Also, every 'ConfiguredPackage' in the
-- plan has to have a valid configuration (see 'configuredPackageValid').
--
-- * if the result is @False@ use 'problems' to get a detailed list.
--
valid :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
          HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
      => FakeMap -> Bool
      -> PlanIndex ipkg srcpkg iresult ifailure
      -> Bool
valid fakeMap indepGoals index =
    null $ problems fakeMap indepGoals index

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
problems :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
             HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
         => FakeMap -> Bool
         -> PlanIndex ipkg srcpkg iresult ifailure
         -> [PlanProblem ipkg srcpkg iresult ifailure]
problems fakeMap indepGoals index =

     [ PackageMissingDeps pkg
       (catMaybes
        (map
         (fmap packageId . PlanIndex.fakeLookupInstalledPackageId fakeMap index)
         missingDeps))
     | (pkg, missingDeps) <- PlanIndex.brokenPackages fakeMap index ]

  ++ [ PackageCycle cycleGroup
     | cycleGroup <- PlanIndex.dependencyCycles fakeMap index ]

  ++ [ PackageInconsistency name inconsistencies
     | (name, inconsistencies) <-
       PlanIndex.dependencyInconsistencies fakeMap indepGoals index ]

  ++ [ PackageStateInvalid pkg pkg'
     | pkg <- PackageIndex.allPackages index
     , Just pkg' <- map (PlanIndex.fakeLookupInstalledPackageId fakeMap index)
                    (CD.nonSetupDeps (depends pkg))
     , not (stateDependencyRelation pkg pkg') ]

-- | The graph of packages (nodes) and dependencies (edges) must be acyclic.
--
-- * if the result is @False@ use 'PackageIndex.dependencyCycles' to find out
--   which packages are involved in dependency cycles.
--
acyclic :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
            HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
        => FakeMap -> PlanIndex ipkg srcpkg iresult ifailure -> Bool
acyclic fakeMap = null . PlanIndex.dependencyCycles fakeMap

-- | An installation plan is closed if for every package in the set, all of
-- its dependencies are also in the set. That is, the set is closed under the
-- dependency relation.
--
-- * if the result is @False@ use 'PackageIndex.brokenPackages' to find out
--   which packages depend on packages not in the index.
--
closed :: (HasInstalledPackageId ipkg, PackageFixedDeps ipkg,
           PackageFixedDeps srcpkg)
       => FakeMap -> PlanIndex ipkg srcpkg iresult ifailure -> Bool
closed fakeMap = null . PlanIndex.brokenPackages fakeMap

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
consistent :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
               HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
           => FakeMap -> PlanIndex ipkg srcpkg iresult ifailure -> Bool
consistent fakeMap = null . PlanIndex.dependencyInconsistencies fakeMap False

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


-- | Compute the dependency closure of a _source_ package in a install plan
--
-- See `Distribution.Client.PlanIndex.dependencyClosure`
dependencyClosure :: (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
                      HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
                  => GenericInstallPlan ipkg srcpkg iresult ifailure
                  -> [PackageIdentifier]
                  -> Either [(GenericPlanPackage ipkg srcpkg iresult ifailure,
                              [InstalledPackageId])]
                            (PackageIndex
                             (GenericPlanPackage ipkg srcpkg iresult ifailure))
dependencyClosure installPlan pids =
    PlanIndex.dependencyClosure
      (planFakeMap installPlan)
      (planIndex installPlan)
      (map (resolveFakeId . fakeInstalledPackageId) pids)
  where
    resolveFakeId :: InstalledPackageId -> InstalledPackageId
    resolveFakeId ipid = Map.findWithDefault ipid ipid (planFakeMap installPlan)
