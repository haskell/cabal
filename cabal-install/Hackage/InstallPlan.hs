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
  valid,
  complete,
  consistent,
  
  new,
  toList,
  done,
  next,
  completed,
  failed
  ) where

import Hackage.Types
         ( PkgInfo(pkgDesc), FlagAssignment )
import Distribution.Package
         ( PackageIdentifier(..), Package(..), PackageFixedDeps(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.PackageDescription
         ( GenericPackageDescription(genPackageFlags)
         , PackageDescription(buildDepends)
         , Flag(MkFlag, flagName) )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Simple.PackageIndex
         ( PackageIndex, insertPackage)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Utils
         ( comparing, equating )
import Distribution.Text
         ( display )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId(..) )

import Data.List
         ( sort, sortBy, groupBy )
import Data.Maybe
         ( isJust )
import qualified Data.Graph as Graph
          ( SCC(..), stronglyConnCompR )
import Control.Exception
         ( assert )
import Debug.Trace
         ( trace )

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
-- The goal is to calculate an installation plan that is acyclic, consistent
-- and complete.
--
-- An installation plan is a set of packages that are going to be used
-- together. It will consist of a mixture of installed packages and available
-- packages along with their exact version dependencies. An installation plan
-- is complete if for every package in the set, all of its dependencies are
-- also in the set. It is consistent if for every package in the set, all
-- dependencies which target that package have the same version.

data ConfiguredPackage = ConfiguredPackage
       PkgInfo             -- ^ package info, including repo
       FlagAssignment      -- ^ complete flag assignment for the package
       [PackageIdentifier] -- ^ exact dependencies, must be consistent with the
                           -- version constraints in the package info
  deriving Show

instance Package ConfiguredPackage where
  packageId (ConfiguredPackage pkg _ _) = packageId pkg

instance PackageFixedDeps ConfiguredPackage where
  depends (ConfiguredPackage _ _ deps) = deps

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
    planOS       :: OS,
    planArch     :: Arch,
    planCompiler :: CompilerId
  } 
  deriving Show

toList :: InstallPlan buildResult -> [PlanPackage buildResult]
toList = PackageIndex.allPackages . planIndex

-- A valid installation plan is a set of packages that is 'acyclic', 'complete'
-- and 'consistent'.
--
valid :: InstallPlan buildResult -> Bool
valid plan = noDuplicates plan
          && acyclic      plan
          && complete     plan
          && consistent   plan
          && validConfig  plan

-- | It is supposed to be a set afterall so each package in the plan must be
-- unique by its id.
--
noDuplicates :: InstallPlan buildResult -> Bool
noDuplicates =
    all ((== 1) . length)
  . groupBy (equating packageId)
  . sortBy  (comparing packageId)
  . toList

acyclic :: InstallPlan buildResult -> Bool
acyclic plan =
  null [ vs
       | Graph.CyclicSCC vs <- Graph.stronglyConnCompR
                                 [ (pkg, packageId pkg, depends pkg)
                                 | pkg <- PackageIndex.allPackages (planIndex plan) ] ]

-- | An installation plan is complete if for every package in the set, all of
-- its dependencies are also in the set.
--
complete :: InstallPlan buildResult -> Bool
complete (InstallPlan { planIndex = index}) =
  all (isJust . PackageIndex.lookupPackageId index)
    (concatMap depends (PackageIndex.allPackages index))

-- An installation plan is consistent if for every package in the set, all
-- dependencies which target that package have the same version.
consistent :: InstallPlan buildResult -> Bool
consistent (InstallPlan { planIndex = index}) =
    all same
  . map (map snd)
  . groupBy (equating  fst)
  . sortBy  (comparing fst)
  $ [ (name, [version])
    | pkg <- PackageIndex.allPackages index
    , PackageIdentifier name version <- depends pkg ]
  where
    same :: Eq a => [a] -> Bool
    same xs = and (zipWith (==) xs (tail xs))

validConfig :: InstallPlan buildResult -> Bool
validConfig (InstallPlan index os arch comp) =
  flip all [ pkg | Configured pkg <- PackageIndex.allPackages index ] $
    \(ConfiguredPackage pkginfo flags deps) ->
         flagsTotal (pkgDesc pkginfo) flags
      && depsValid  (pkgDesc pkginfo) flags deps
  
  where
    flagsTotal :: GenericPackageDescription -> FlagAssignment -> Bool
    flagsTotal pkg flags =
         sort [ name | (name,_) <- flags ]
      == sort [ name | MkFlag { flagName = name } <- genPackageFlags pkg ]

    depsValid :: GenericPackageDescription -> FlagAssignment -> [PackageIdentifier] -> Bool
    depsValid pkg flags deps =
      --TODO: use something lower level than finalizePackageDescription
      case finalizePackageDescription flags (Nothing :: Maybe (PackageIndex PackageIdentifier)) os arch comp [] pkg of
        Right (pkg', _) -> flip all (buildDepends pkg') $ \dep ->
          case PackageIndex.lookupDependency index' dep of
            [_] -> True
            _   -> False
        _ -> False
      where index' = PackageIndex.fromList deps

-- | Build an installation plan from a valid set of resolved packages.
--
new :: OS -> Arch -> CompilerId -> [PlanPackage buildResult] -> InstallPlan buildResult
new os arch compiler pkgs =
  let plan = InstallPlan (PackageIndex.fromList pkgs) os arch compiler
   in if valid plan
        then plan
        else error "InstallPlan.new: invalid plan"

-- | Is the plan completed?
--
done :: InstallPlan buildResult -> Bool
done (InstallPlan { planIndex = index}) =
  null [ () | Configured _ <- PackageIndex.allPackages index ]

-- | The next package, meaning a package which has no dependencies.
--
-- * The graph must not be 'done'.
--
next :: InstallPlan buildResult -> ConfiguredPackage
next plan@(InstallPlan { planIndex = index }) = assert (valid plan) $
  let allReadyPackages =
        [ pkg
        | Configured pkg <- PackageIndex.allPackages index
        , flip all (depends pkg) $ \dep ->
            case PackageIndex.lookupPackageId index dep of
              Just (Configured  _) -> False
              Just (Failed    _ _) -> False
              Just (PreExisting _) -> True
              Just (Installed   _) -> True
              Nothing -> error "InstallPlan.next: incomplete install plan" ]
  in case allReadyPackages of
    []      -> error $ "InstallPlan.next: internal error: no nodes with 0-outdegree\n"
                    ++ unlines (map (display . packageId) (PackageIndex.allPackages index))
    (pkg:_) -> pkg

-- | Marks a package in the graph as completed. Also saves the build result for
-- the completed package in the plan.
--
-- * The package must exist in the graph.
-- * The package must have had no uninstalled dependent packages.
--
completed :: PackageIdentifier
          -> InstallPlan buildResult -> InstallPlan buildResult
completed pkgid plan =
  case PackageIndex.lookupPackageId index pkgid of
    Just (Configured cp) -> plan { planIndex = PackageIndex.insertPackage index (Installed cp) }
    _ -> error "InstallPlan.completed: internal error; cannot mark package as completed"
  where index = planIndex plan

-- | Marks a package in the graph as having failed. It also marks all the
-- packages that depended on it.
--
--
-- * The package must exist in the graph.
--
failed :: PackageIdentifier -> buildResult -> buildResult
             -> InstallPlan buildResult -> InstallPlan buildResult
failed pkgid0 buildResult dependentBuildResult plan =
  case PackageIndex.lookupPackageId index0 pkgid0 of
    Just (Configured cp) ->
      let index = PackageIndex.insertPackage index0 (Failed cp buildResult)
      in plan { planIndex = markDepsAsFailed pkgid0 index }
    _ -> error ""
  where
  index0 = planIndex plan
  --markDepsAsFailed :: PackageIdentifier -> PackageIndex br -> PackageIndex br
  markDepsAsFailed pkgid index =
    case PackageIndex.lookupPackageId index pkgid of
      Just (Configured cp) ->
        let index1 = PackageIndex.insertPackage index (Failed cp dependentBuildResult)
            deps = depends cp
        in foldr markDepsAsFailed index1 deps
      _ -> index
