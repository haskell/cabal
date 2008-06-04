-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.InstallPlan
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Package installation plan
--
-----------------------------------------------------------------------------
module Hackage.InstallPlan (
  InstallPlan,
  ConfiguredPackage(..),
  PlanPackage(..),

  -- * Operations on 'InstallPlan's
  new,
  toList,
  ready,
  completed,
  failed,

  -- * Checking valididy of plans
  valid,
  closed,
  consistent,
  acyclic,
  configuredPackageValid,

  -- ** Details on invalid plans
  PlanProblem(..),
  showPlanProblem,
  PackageProblem(..),
  showPackageProblem,
  problems,
  configuredPackageProblems
  ) where

import Hackage.Types
         ( AvailablePackage(packageDescription), ConfiguredPackage(..) )
import Distribution.Package
         ( PackageIdentifier(..), Package(..), PackageFixedDeps(..)
         , packageName, Dependency(..) )
import Distribution.Version
         ( Version, withinRange )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.PackageDescription
         ( GenericPackageDescription(genPackageFlags)
         , PackageDescription(buildDepends)
         , Flag(flagName), FlagName(..) )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Simple.PackageIndex
         ( PackageIndex )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Text
         ( display )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId(..) )
import Hackage.Utils
         ( duplicates, duplicatesBy, mergeBy, MergeResult(..) )
import Distribution.Simple.Utils
         ( comparing, intercalate )

import Data.List
         ( sort, sortBy )
import Data.Maybe
         ( fromMaybe )
import qualified Data.Graph as Graph
import Data.Graph (Graph)
import Control.Exception
         ( assert )

-- When cabal tries to install a number of packages, including all their
-- dependencies it has a non-trivial problem to solve.
--
-- The Problem:
--
-- In general we start with a set of installed packages and a set of available
-- packages.
--
-- Installed packages have fixed dependencies. They have already been built and
-- we know exactly what packages they were built against, including their exact
-- versions. 
--
-- Available package have somewhat flexible dependencies. They are specified as
-- version ranges, though really they're predicates. To make matters worse they
-- have conditional flexible dependencies. Configuration flags can affect which
-- packages are required and can place additional constraints on their
-- versions.
--
-- These two sets of package can and usually do overlap. There can be installed
-- packages that are also available which means they could be re-installed if
-- required, though there will also be packages which are not available and
-- cannot be re-installed. Very often there will be extra versions available
-- than are installed. Sometimes we may like to prefer installed packages over
-- available ones or perhaps always prefer the latest available version whether
-- installed or not.
--
-- The goal is to calculate an installation plan that is closed, acyclic and
-- consistent and where every configured package is valid.
--
-- An installation plan is a set of packages that are going to be used
-- together. It will consist of a mixture of installed packages and available
-- packages along with their exact version dependencies. An installation plan
-- is closed if for every package in the set, all of its dependencies are
-- also in the set. It is consistent if for every package in the set, all
-- dependencies which target that package have the same version.

-- Note that plans do not necessarily compose. You might have a valid plan for
-- package A and a valid plan for package B. That does not mean the composition
-- is simultaniously valid for A and B. In particular you're most likely to
-- have problems with inconsistent dependencies.
-- On the other hand it is true that every closed sub plan is valid.

data PlanPackage buildResult = PreExisting InstalledPackageInfo
                             | Configured  ConfiguredPackage
                             | Installed   ConfiguredPackage
                             | Failed      ConfiguredPackage buildResult
  deriving Show

instance Package (PlanPackage buildResult) where
  packageId (PreExisting pkg) = packageId pkg
  packageId (Configured pkg)  = packageId pkg
  packageId (Installed pkg)   = packageId pkg
  packageId (Failed pkg _)    = packageId pkg

instance PackageFixedDeps (PlanPackage buildResult) where
  depends (PreExisting pkg) = depends pkg
  depends (Configured pkg)  = depends pkg
  depends (Installed pkg)   = depends pkg
  depends (Failed pkg _)    = depends pkg

data InstallPlan buildResult = InstallPlan {
    planIndex    :: PackageIndex (PlanPackage buildResult),
    planGraph    :: Graph,
    planGraphRev :: Graph,
    planPkgIdOf  :: Graph.Vertex -> PackageIdentifier,
    planVertexOf :: PackageIdentifier -> Graph.Vertex,
    planOS       :: OS,
    planArch     :: Arch,
    planCompiler :: CompilerId
  }

invariant :: InstallPlan a -> Bool
invariant plan =
  valid (planOS plan) (planArch plan) (planCompiler plan) (planIndex plan)

internalError :: String -> a
internalError msg = error $ "InstallPlan: internal error: " ++ msg

-- | Build an installation plan from a valid set of resolved packages.
--
new :: OS -> Arch -> CompilerId -> PackageIndex (PlanPackage a)
    -> Either [PlanProblem a] (InstallPlan a)
new os arch compiler index =
  case problems os arch compiler index of
    [] -> Right InstallPlan {
            planIndex    = index,
            planGraph    = graph,
            planGraphRev = Graph.transposeG graph,
            planPkgIdOf  = vertexToPkgId,
            planVertexOf = fromMaybe noSuchPkgId . pkgIdToVertex,
            planOS       = os,
            planArch     = arch,
            planCompiler = compiler
          }
      where (graph, vertexToPkgId, pkgIdToVertex) =
              PackageIndex.dependencyGraph index
            noSuchPkgId = internalError "package is not in the graph"
    probs -> Left probs

toList :: InstallPlan buildResult -> [PlanPackage buildResult]
toList = PackageIndex.allPackages . planIndex

-- | The packages that are ready to be installed. That is they are in the
-- configured state and have all their dependencies installed already.
-- The plan is complete if the result is @[]@.
--
ready :: InstallPlan buildResult -> [ConfiguredPackage]
ready plan = assert check readyPackages
  where
    check = if null readyPackages then null configuredPackages else True
    configuredPackages =
      [ pkg | Configured pkg <- PackageIndex.allPackages (planIndex plan) ]
    readyPackages = filter (all isInstalled . depends) configuredPackages
    isInstalled pkg =
      case PackageIndex.lookupPackageId (planIndex plan) pkg of
        Just (Configured  _) -> False
        Just (Failed    _ _) -> internalError depOnFailed
        Just (PreExisting _) -> True
        Just (Installed   _) -> True
        Nothing              -> internalError incomplete
    incomplete  = "install plan is not closed"
    depOnFailed = "configured package depends on failed package"

-- | Marks a package in the graph as completed. Also saves the build result for
-- the completed package in the plan.
--
-- * The package must exist in the graph.
-- * The package must have had no uninstalled dependent packages.
--
completed :: PackageIdentifier
          -> InstallPlan buildResult -> InstallPlan buildResult
completed pkgid plan = assert (invariant plan') plan'
  where
    plan'     = plan {
                  planIndex = PackageIndex.insert installed (planIndex plan)
                }
    installed = Installed (lookupConfiguredPackage plan pkgid)

-- | Marks a package in the graph as having failed. It also marks all the
-- packages that depended on it as having failed.
--
-- * The package must exist in the graph and be in the configured state.
--
failed :: PackageIdentifier -- ^ The id of the package that failed to install
       -> buildResult       -- ^ The build result to use for the failed package
       -> buildResult       -- ^ The build result to use for its dependencies
       -> InstallPlan buildResult
       -> InstallPlan buildResult
failed pkgid buildResult buildResult' plan = assert (invariant plan') plan'
  where
    plan'    = plan {
                 planIndex = PackageIndex.merge (planIndex plan) failures
               }
    pkg      = lookupConfiguredPackage plan pkgid
    failures = PackageIndex.fromList
             $ Failed pkg buildResult
             : [ Failed pkg' buildResult'
               | Just pkg' <- map (lookupConfiguredPackage' plan)
                            $ packagesThatDependOn plan pkgid ]

-- | lookup the reachable packages in the reverse dependency graph
--
packagesThatDependOn :: InstallPlan a
                     -> PackageIdentifier -> [PackageIdentifier]
packagesThatDependOn plan = map (planPkgIdOf plan)
                          . Graph.reachable (planGraphRev plan)
                          . planVertexOf plan

-- | lookup a package that we expect to be in the configured state
--
lookupConfiguredPackage :: InstallPlan a
                        -> PackageIdentifier -> ConfiguredPackage
lookupConfiguredPackage plan pkgid =
  case PackageIndex.lookupPackageId (planIndex plan) pkgid of
    Just (Configured pkg) -> pkg
    _  -> internalError $ "not configured or no such pkg " ++ display pkgid

-- | lookup a package that we expect to be in the configured or failed state
--
lookupConfiguredPackage' :: InstallPlan a
                         -> PackageIdentifier -> Maybe ConfiguredPackage
lookupConfiguredPackage' plan pkgid =
  case PackageIndex.lookupPackageId (planIndex plan) pkgid of
    Just (Configured pkg)  -> Just pkg
    Just (Failed _ reason) -> Nothing
    _  -> internalError $ "not configured or no such pkg " ++ display pkgid

-- ------------------------------------------------------------
-- * Checking valididy of plans
-- ------------------------------------------------------------

-- | A valid installation plan is a set of packages that is 'acyclic',
-- 'closed' and 'consistent'. Also, every 'ConfiguredPackage' in the
-- plan has to have a valid configuration (see 'configuredPackageValid').
--
-- * if the result is @False@ use 'problems' to get a detailed list.
--
valid :: OS -> Arch -> CompilerId -> PackageIndex (PlanPackage a) -> Bool
valid os arch comp index = null (problems os arch comp index)

data PlanProblem a =
     PackageInvalid       ConfiguredPackage [PackageProblem]
   | PackageMissingDeps   (PlanPackage a) [PackageIdentifier]
   | PackageCycle         [PlanPackage a]
   | PackageInconsistency String [(PackageIdentifier, Version)]
   | PackageStateInvalid  (PlanPackage a) (PlanPackage a)

showPlanProblem :: PlanProblem a -> String
showPlanProblem (PackageInvalid pkg packageProblems) =
     "Package " ++ display (packageId pkg)
  ++ " has an invalid configuration, in particular:\n"
  ++ unlines [ "  " ++ showPackageProblem problem
             | problem <- packageProblems ]

showPlanProblem (PackageMissingDeps pkg missingDeps) =
     "Package " ++ display (packageId pkg)
  ++ " depends on the following packages which are missing from the plan "
  ++ intercalate ", " (map display missingDeps)

showPlanProblem (PackageCycle cycleGroup) =
     "The following packages are involved in a dependency cycle "
  ++ intercalate ", " (map (display.packageId) cycleGroup)

showPlanProblem (PackageInconsistency name inconsistencies) =
     "Package " ++ name
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
    showPlanState (PreExisting _) = "pre-existing"
    showPlanState (Configured  _) = "configured"
    showPlanState (Installed   _) = "installed"
    showPlanState (Failed    _ _) = "failed"

-- | For an invalid plan, produce a detailed list of problems as human readable
-- error messages. This is mainly intended for debugging purposes.
-- Use 'showPlanProblem' for a human readable explanation.
--
problems :: OS -> Arch -> CompilerId
         -> PackageIndex (PlanPackage a) -> [PlanProblem a]
problems os arch comp index =
     [ PackageInvalid pkg packageProblems
     | Configured pkg <- PackageIndex.allPackages index
     , let packageProblems = configuredPackageProblems os arch comp pkg
     , not (null packageProblems) ]

  ++ [ PackageMissingDeps pkg missingDeps
     | (pkg, missingDeps) <- PackageIndex.brokenPackages index ]

  ++ [ PackageCycle cycleGroup
     | cycleGroup <- PackageIndex.dependencyCycles index ]

  ++ [ PackageInconsistency name inconsistencies
     | (name, inconsistencies) <- PackageIndex.dependencyInconsistencies index ]

  ++ [ PackageStateInvalid pkg pkg'
     | pkg <- PackageIndex.allPackages index
     , Just pkg' <- map (PackageIndex.lookupPackageId index) (depends pkg)
     , not (stateDependencyRelation pkg pkg') ]

-- | The graph of packages (nodes) and dependencies (edges) must be acyclic.
--
-- * if the result is @False@ use 'PackageIndex.dependencyCycles' to find out
--   which packages are involved in dependency cycles.
--
acyclic :: PackageIndex (PlanPackage a) -> Bool
acyclic = null . PackageIndex.dependencyCycles

-- | An installation plan is closed if for every package in the set, all of
-- its dependencies are also in the set. That is, the set is closed under the
-- dependency relation.
--
-- * if the result is @False@ use 'PackageIndex.brokenPackages' to find out
--   which packages depend on packages not in the index.
--
closed :: PackageIndex (PlanPackage a) -> Bool
closed = null . PackageIndex.brokenPackages

-- | An installation plan is consistent if all dependencies that target a
-- single package name, target the same version.
--
-- This is slightly subtle. It is not the same as requiring that there be at
-- most one version of any package in the set. It only requires that of
-- packages which have more than one other package depending on them. We could
-- actually make the condition even more precise and say that different
-- versions are ok so long as they are not both in the transative closure of
-- any other package (or equivalently that their inverse closures do not
-- intersect). The point is we do not want to have any packages depending
-- directly or indirectly on two different versions of the same package. The
-- current definition is just a safe aproximation of that.
--
-- * if the result is @False@ use 'PackageIndex.dependencyInconsistencies' to
--   find out which packages are.
--
consistent :: PackageIndex (PlanPackage a) -> Bool
consistent = null . PackageIndex.dependencyInconsistencies

-- | The states of packages have that depend on each other must respect
-- this relation. That is for very case where package @a@ depends on
-- package @b@ we require that @dependencyStatesOk a b = True@.
--
stateDependencyRelation :: PlanPackage a -> PlanPackage a -> Bool
stateDependencyRelation (PreExisting _) (PreExisting _) = True

stateDependencyRelation (Configured  _) (PreExisting _) = True
stateDependencyRelation (Configured  _) (Configured  _) = True
stateDependencyRelation (Configured  _) (Installed   _) = True

stateDependencyRelation (Installed   _) (PreExisting _) = True
stateDependencyRelation (Installed   _) (Installed   _) = True

stateDependencyRelation (Failed    _ _) (PreExisting _) = True
-- failed can depends on configured because a package can depend on
-- several other packages and if one of the deps fail then we fail
-- but we still depend on the other ones that did not fail:
stateDependencyRelation (Failed    _ _) (Configured  _) = True
stateDependencyRelation (Failed    _ _) (Installed   _) = True
stateDependencyRelation (Failed    _ _) (Failed    _ _) = True

stateDependencyRelation _               _               = False

-- | A 'ConfiguredPackage' is valid if the flag assignment is total and if
-- in the configuration given by the flag assignment, all the package
-- dependencies are satisfied by the specified packages.
--
configuredPackageValid :: OS -> Arch -> CompilerId -> ConfiguredPackage -> Bool
configuredPackageValid os arch comp pkg =
  null (configuredPackageProblems os arch comp pkg)

data PackageProblem = DuplicateFlag FlagName
                    | MissingFlag   FlagName
                    | ExtraFlag     FlagName
                    | DuplicateDeps [PackageIdentifier]
                    | MissingDep    Dependency
                    | ExtraDep      PackageIdentifier
                    | InvalidDep    Dependency PackageIdentifier

showPackageProblem :: PackageProblem -> String
showPackageProblem (DuplicateFlag (FlagName flag)) =
  "duplicate flag in the flag assignment: " ++ flag

showPackageProblem (MissingFlag (FlagName flag)) =
  "missing an assignment for the flag: " ++ flag

showPackageProblem (ExtraFlag (FlagName flag)) =
  "extra flag given that is not used by the package: " ++ flag

showPackageProblem (DuplicateDeps pkgids) =
     "duplicate packages specified as selected dependencies: "
  ++ intercalate ", " (map display pkgids)

showPackageProblem (MissingDep dep) =
     "the package has a dependency " ++ display dep
  ++ " but no package has been selected to satisfy it."

showPackageProblem (ExtraDep pkgid) =
     "the package configuration specifies " ++ display pkgid
  ++ " but (with the given flag assignment) the package does not actually"
  ++ " depend on any version of that package."

showPackageProblem (InvalidDep dep pkgid) =
     "the package depends on " ++ display dep
  ++ " but the configuration specifies " ++ display pkgid
  ++ " which does not satisfy the dependency."

configuredPackageProblems :: OS -> Arch -> CompilerId
                          -> ConfiguredPackage -> [PackageProblem]
configuredPackageProblems os arch comp
  (ConfiguredPackage pkg specifiedFlags specifiedDeps) =
     [ DuplicateFlag flag | ((flag,_):_) <- duplicates specifiedFlags ]
  ++ [ MissingFlag flag | OnlyInLeft  flag <- mergedFlags ]
  ++ [ ExtraFlag   flag | OnlyInRight flag <- mergedFlags ]
  ++ [ DuplicateDeps pkgs
     | pkgs <- duplicatesBy (comparing packageName) specifiedDeps ]
  ++ [ MissingDep dep       | OnlyInLeft  dep       <- mergedDeps ]
  ++ [ ExtraDep       pkgid | OnlyInRight     pkgid <- mergedDeps ]
  ++ [ InvalidDep dep pkgid | InBoth      dep pkgid <- mergedDeps
                            , not (packageSatisfiesDependency pkgid dep) ]
  where
    mergedFlags = mergeBy compare
      (sort $ map flagName (genPackageFlags (packageDescription pkg)))
      (sort $ map fst specifiedFlags)

    mergedDeps = mergeBy
      (\dep pkgid -> dependencyName dep `compare` packageName pkgid)
      (sortBy (comparing dependencyName) requiredDeps)
      (sortBy (comparing packageName)    specifiedDeps)

    packageSatisfiesDependency
      (PackageIdentifier name  version)
      (Dependency        name' versionRange) = assert (name == name') $
        version `withinRange` versionRange

    dependencyName (Dependency name _) = name

    requiredDeps :: [Dependency]
    requiredDeps =
      --TODO: use something lower level than finalizePackageDescription
      case finalizePackageDescription specifiedFlags
         (Nothing :: Maybe (PackageIndex PackageIdentifier)) os arch comp []
         (packageDescription pkg) of
        Right (resolvedPkg, _) -> buildDepends resolvedPkg
        Left  _ -> error "configuredPackageInvalidDeps internal error"
