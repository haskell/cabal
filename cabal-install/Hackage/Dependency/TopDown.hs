-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Dependency.Types
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Common types for dependency resolution.
-----------------------------------------------------------------------------
module Hackage.Dependency.TopDown (
    topDownResolver
  ) where

import Hackage.Dependency.TopDown.Types
import qualified Hackage.Dependency.TopDown.Constraints as Constraints
import Hackage.Dependency.TopDown.Constraints
         ( Satisfiable(..) )
import qualified Hackage.InstallPlan as InstallPlan
import Hackage.InstallPlan
         ( PlanPackage(..) )
import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..)
         , ConfiguredPackage(..) )
import Hackage.Dependency.Types
         ( PackageName, DependencyResolver, PackageVersionPreference(..)
         , Progress(..), foldProgress )

import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( PackageIdentifier, Package(packageId), packageVersion, packageName
         , Dependency(Dependency), thisPackageVersion, notThisPackageVersion
         , PackageFixedDeps(depends) )
import Distribution.PackageDescription
         ( PackageDescription(buildDepends) )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription, flattenPackageDescription )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( OS, Arch )
import Distribution.Simple.Utils
         ( equating, comparing )
import Distribution.Text
         ( display )

import Data.List
         ( foldl', maximumBy, minimumBy, deleteBy, nub, sort )
import Data.Maybe
         ( fromJust )
import Data.Monoid
         ( Monoid(mempty) )
import Control.Monad
         ( guard )
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Graph as Graph
import qualified Data.Array as Array

-- ------------------------------------------------------------
-- * Search state types
-- ------------------------------------------------------------

type Constraints  = Constraints.Constraints
                      InstalledPackage UnconfiguredPackage ExclusionReason
type SelectedPackages = PackageIndex SelectedPackage

-- ------------------------------------------------------------
-- * The search tree type
-- ------------------------------------------------------------

data SearchSpace inherited pkg
   = ChoiceNode inherited [[(pkg, SearchSpace inherited pkg)]]
   | Failure Failure

-- ------------------------------------------------------------
-- * Traverse a search tree
-- ------------------------------------------------------------

explore :: (PackageName -> PackageVersionPreference)
        -> SearchSpace a SelectablePackage
        -> Progress Log Failure a

explore _    (Failure failure)      = Fail failure
explore _explore    (ChoiceNode result []) = Done result
explore pref (ChoiceNode _ choices) =
  case [ choice | [choice] <- choices ] of
    ((pkg, node'):_) -> Step (Select pkg [])    (explore pref node')
    []               -> seq pkgs' -- avoid retaining defaultChoice
                      $ Step (Select pkg pkgs') (explore pref node')
      where
        choice       = minimumBy (comparing topSortNumber) choices
        pkgname      = packageName . fst . head $ choice
        (pkg, node') = maximumBy (bestByPref pkgname) choice
        pkgs' = deleteBy (equating packageId) pkg (map fst choice)

  where
    topSortNumber choice = case fst (head choice) of
      InstalledOnly           (InstalledPackage  _ i _) -> i
      AvailableOnly           (UnconfiguredPackage _ i) -> i
      InstalledAndAvailable _ (UnconfiguredPackage _ i) -> i

    bestByPref pkgname = case pref pkgname of
      PreferLatest    -> comparing (\(p,_) ->                 packageId p)
      PreferInstalled -> comparing (\(p,_) -> (isInstalled p, packageId p))
      where isInstalled (AvailableOnly _) = False
            isInstalled _                 = True

-- ------------------------------------------------------------
-- * Generate a search tree
-- ------------------------------------------------------------

type ConfigurePackage = PackageIndex SelectablePackage
                     -> SelectablePackage
                     -> Either [Dependency] SelectedPackage

searchSpace :: ConfigurePackage
            -> Constraints
            -> SelectedPackages
            -> Set PackageName
            -> SearchSpace (SelectedPackages, Constraints) SelectablePackage
searchSpace configure constraints selected next =
  ChoiceNode (selected, constraints)
    [ [ (pkg, select name pkg)
      | pkg <- PackageIndex.lookupPackageName available name ]
    | name <- Set.elems next ]
  where
    available = Constraints.choices constraints

    select name pkg = case configure available pkg of
      Left missing -> Failure $ ConfigureFailed pkg
                        [ (dep, Constraints.conflicting constraints dep)
                        | dep <- missing ]
      Right pkg' ->
        let selected' = PackageIndex.insert pkg' selected
            newPkgs   = [ name'
                        | dep <- packageConstraints pkg'
                        , let (Dependency name' _) = untagDependency dep
                        , null (PackageIndex.lookupPackageName selected' name') ]
            newDeps   = packageConstraints pkg'
            next'     = Set.delete name
                      $ foldl' (flip Set.insert) next newPkgs
         in case constrainDeps pkg' newDeps constraints of
              Left failure       -> Failure failure
              Right constraints' -> searchSpace configure
                                      constraints' selected' next'

packageConstraints :: SelectedPackage -> [TaggedDependency]
packageConstraints = either installedConstraints availableConstraints
                   . preferAvailable
  where
    preferAvailable (InstalledOnly           pkg) = Left pkg
    preferAvailable (AvailableOnly           pkg) = Right pkg
    preferAvailable (InstalledAndAvailable _ pkg) = Right pkg
    installedConstraints (InstalledPackage      _ _ deps) =
      [ TaggedDependency InstalledConstraint (thisPackageVersion dep)
      | dep <- deps ]
    availableConstraints (SemiConfiguredPackage _ _ deps) =
      [ TaggedDependency NoInstalledConstraint dep | dep <- deps ]

constrainDeps :: SelectedPackage -> [TaggedDependency] -> Constraints
              -> Either Failure Constraints
constrainDeps pkg []         cs =
  case addPackageSelectConstraint (packageId pkg) cs of
    Satisfiable cs' -> Right cs'
    _               -> impossible
constrainDeps pkg (dep:deps) cs =
  case addPackageDependencyConstraint (packageId pkg) dep cs of
    Satisfiable cs' -> constrainDeps pkg deps cs'
    Unsatisfiable   -> impossible
    ConflictsWith conflicts ->
      Left (DependencyConflict pkg dep conflicts)

-- ------------------------------------------------------------
-- * The main algorithm
-- ------------------------------------------------------------

search :: ConfigurePackage
       -> (PackageName -> PackageVersionPreference)
       -> Constraints
       -> Set PackageName
       -> Progress Log Failure (SelectedPackages, Constraints)
search configure pref constraints =
  explore pref . searchSpace configure constraints mempty

-- ------------------------------------------------------------
-- * The top level resolver
-- ------------------------------------------------------------

-- | The main exported resolver, with string logging and failure types to fit
-- the standard 'DependencyResolver' interface.
--
topDownResolver :: DependencyResolver a
topDownResolver = ((((((mapMessages .).).).).).) . topDownResolver'
  where
    mapMessages :: Progress Log Failure a -> Progress String String a
    mapMessages = foldProgress (Step . showLog) (Fail . showFailure) Done

-- | The native resolver with detailed structured logging and failure types.
--
topDownResolver' :: OS -> Arch -> CompilerId
                 -> PackageIndex InstalledPackageInfo
                 -> PackageIndex AvailablePackage
                 -> (PackageName -> PackageVersionPreference)
                 -> [UnresolvedDependency]
                 -> Progress Log Failure [PlanPackage a]
topDownResolver' os arch comp installed available pref deps =
      fmap (uncurry finalise)
    . (\cs -> search configure pref cs initialPkgNames)
  =<< constrainTopLevelDeps deps constraints

  where
    configure   = configurePackage os arch comp
    constraints = Constraints.empty
                    (annotateInstalledPackages topSortNumber installed)
                    (annotateAvailablePackages topSortNumber available)
    topSortNumber = topologicalSortNumbering installed available

    initialDeps     = [ dep  | UnresolvedDependency dep _ <- deps ]
    initialPkgNames = Set.fromList [ name | Dependency name _ <- initialDeps ]

    finalise selected = PackageIndex.allPackages
                      . improvePlan installed
                      . PackageIndex.fromList
                      . finaliseSelectedPackages selected

constrainTopLevelDeps :: [UnresolvedDependency] -> Constraints
                      -> Progress a Failure Constraints
constrainTopLevelDeps []                                cs = Done cs
constrainTopLevelDeps (UnresolvedDependency dep _:deps) cs =
  case addTopLevelDependencyConstraint dep cs of
    Satisfiable cs'         -> constrainTopLevelDeps deps cs'
    Unsatisfiable           -> Fail (TopLevelDependencyUnsatisfiable dep)
    ConflictsWith conflicts -> Fail (TopLevelDependencyConflict dep conflicts)

configurePackage :: OS -> Arch -> CompilerId -> ConfigurePackage
configurePackage os arch comp available spkg = case spkg of
  InstalledOnly         ipkg      -> Right (InstalledOnly ipkg)
  AvailableOnly              apkg -> fmap AvailableOnly (configure apkg)
  InstalledAndAvailable ipkg apkg -> fmap (InstalledAndAvailable ipkg)
                                          (configure apkg)
  where
  configure (UnconfiguredPackage apkg@(AvailablePackage _ p _) _) =
    case finalizePackageDescription [] (Just available) os arch comp [] p of
      Left missing       -> Left missing
      Right (pkg, flags) -> Right $
        SemiConfiguredPackage apkg flags (buildDepends pkg)

-- | Annotate each installed packages with its set of transative dependencies
-- and its topological sort number.
--
annotateInstalledPackages :: (PackageName -> TopologicalSortNumber)
                          -> PackageIndex InstalledPackageInfo
                          -> PackageIndex InstalledPackage
annotateInstalledPackages dfsNumber installed = PackageIndex.fromList
  [ InstalledPackage pkg (dfsNumber (packageName pkg)) (transitiveDepends pkg)
  | pkg <- PackageIndex.allPackages installed ]
  where
    transitiveDepends :: InstalledPackageInfo -> [PackageIdentifier]
    transitiveDepends = map toPkgid . tail . Graph.reachable graph
                      . fromJust . toVertex . packageId
    (graph, toPkgid, toVertex) = PackageIndex.dependencyGraph installed


-- | Annotate each available packages with its topological sort number.
--
annotateAvailablePackages :: (PackageName -> TopologicalSortNumber)
                          -> PackageIndex AvailablePackage
                          -> PackageIndex UnconfiguredPackage
annotateAvailablePackages dfsNumber available = PackageIndex.fromList
  [ UnconfiguredPackage pkg (dfsNumber (packageName pkg))
  | pkg <- PackageIndex.allPackages available ]

-- | One of the heuristics we use when guessing which path to take in the
-- search space is an ordering on the choices we make. It's generally better
-- to make decisions about packages higer in the dep graph first since they
-- place constraints on packages lower in the dep graph.
--
-- To pick them in that order we annotate each package with its topological
-- sort number. So if package A depends on package B then package A will have
-- a lower topological sort number than B and we'll make a choice about which
-- version of A to pick before we make a choice about B (unless there is only
-- one possible choice for B in which case we pick that immediately).
--
-- To construct these topological sort numbers we combine and flatten the
-- installed and available package sets. We consider only dependencies between
-- named packages, not including versions and for not-yet-configured packages
-- we look at all the possible dependencies, not just those under any single
-- flag assignment. This means we can actually get impossible combinations of
-- edges and even cycles, but that doesn't really matter here, it's only a
-- heuristic.
--
topologicalSortNumbering :: PackageIndex InstalledPackageInfo
                         -> PackageIndex AvailablePackage
                         -> (PackageName -> TopologicalSortNumber)
topologicalSortNumbering installed available =
    \pkgname -> let Just vertex = toVertex pkgname
                 in topologicalSortNumbers Array.! vertex
  where
    topologicalSortNumbers = Array.array (Array.bounds graph)
                                         (zip (Graph.topSort graph) [0..])
    (graph, _, toVertex)   = Graph.graphFromEdges $
         [ ((), packageName pkg, nub deps)
         | pkgs@(pkg:_) <- PackageIndex.allPackagesByName installed
         , let deps = [ packageName dep
                      | pkg' <- pkgs
                      , dep  <- depends pkg' ] ]
      ++ [ ((), packageName pkg, nub deps)
         | pkgs@(pkg:_) <- PackageIndex.allPackagesByName available
         , let deps = [ depName
                      | AvailablePackage _ pkg' _ <- pkgs
                      , Dependency depName _ <-
                          buildDepends (flattenPackageDescription pkg') ] ]

-- ------------------------------------------------------------
-- * Post processing the solution
-- ------------------------------------------------------------

finaliseSelectedPackages :: SelectedPackages
                         -> Constraints
                         -> [PlanPackage a]
finaliseSelectedPackages selected constraints =
  map finaliseSelected (PackageIndex.allPackages selected)
  where
    remainingChoices = Constraints.choices constraints
    finaliseSelected (InstalledOnly         ipkg     ) = finaliseInstalled ipkg
    finaliseSelected (AvailableOnly              apkg) = finaliseAvailable apkg
    finaliseSelected (InstalledAndAvailable ipkg apkg) =
      case PackageIndex.lookupPackageId remainingChoices (packageId ipkg) of
        Nothing                          -> impossible --picked package not in constraints
        Just (AvailableOnly _)           -> impossible --to constrain to avail only
        Just (InstalledOnly _)           -> finaliseInstalled ipkg
        Just (InstalledAndAvailable _ _) -> finaliseAvailable apkg

    finaliseInstalled (InstalledPackage pkg _ _) = InstallPlan.PreExisting pkg
    finaliseAvailable (SemiConfiguredPackage pkg flags deps) =
      InstallPlan.Configured (ConfiguredPackage pkg flags deps')
      where deps' = [ packageId pkg'
                    | dep <- deps
                    , let pkg' = case PackageIndex.lookupDependency selected dep of
                                   [pkg''] -> pkg''
                                   _ -> impossible ]

-- | Improve an existing installation plan by, where possible, swapping
-- packages we plan to install with ones that are already installed.
--
improvePlan :: PackageIndex InstalledPackageInfo
            -> PackageIndex (PlanPackage a)
            -> PackageIndex (PlanPackage a)
improvePlan installed selected = foldl' improve selected
                               $ reverseTopologicalOrder selected
  where
    improve selected' = maybe selected' (flip PackageIndex.insert selected')
                      . improvePkg selected'

    -- The idea is to improve the plan by swapping a configured package for
    -- an equivalent installed one. For a particular package the condition is
    -- that the package be in a configured state, that a the same version be
    -- already installed with the exact same dependencies and all the packages
    -- in the plan that it depends on are in the installed state
    improvePkg selected' pkgid = do
      Configured pkg  <- PackageIndex.lookupPackageId selected' pkgid
      ipkg            <- PackageIndex.lookupPackageId installed pkgid
      guard $ sort (depends pkg) == nub (sort (depends ipkg))
      guard $ all (isInstalled selected') (depends pkg)
      return (PreExisting ipkg)

    isInstalled selected' pkgid =
      case PackageIndex.lookupPackageId selected' pkgid of
        Just (PreExisting _) -> True
        _                    -> False

    reverseTopologicalOrder :: PackageFixedDeps pkg => PackageIndex pkg
                            -> [PackageIdentifier]
    reverseTopologicalOrder index = map toPkgId
                                  . Graph.topSort
                                  . Graph.transposeG
                                  $ graph
      where (graph, toPkgId, _) = PackageIndex.dependencyGraph index

-- ------------------------------------------------------------
-- * Adding and recording constraints
-- ------------------------------------------------------------

addPackageSelectConstraint :: PackageIdentifier -> Constraints
                           -> Satisfiable Constraints ExclusionReason
addPackageSelectConstraint pkgid constraints =
  Constraints.constrain dep reason constraints
  where
    dep    = TaggedDependency NoInstalledConstraint (thisPackageVersion pkgid)
    reason = SelectedOther pkgid

addPackageExcludeConstraint :: PackageIdentifier -> Constraints
                     -> Satisfiable Constraints ExclusionReason
addPackageExcludeConstraint pkgid constraints =
  Constraints.constrain dep reason constraints
  where
    dep    = TaggedDependency NoInstalledConstraint
               (notThisPackageVersion pkgid)
    reason = ExcludedByConfigureFail

addPackageDependencyConstraint :: PackageIdentifier -> TaggedDependency -> Constraints
                               -> Satisfiable Constraints ExclusionReason
addPackageDependencyConstraint pkgid dep constraints =
  Constraints.constrain dep reason constraints
  where
    reason = ExcludedByPackageDependency pkgid dep

addTopLevelDependencyConstraint :: Dependency -> Constraints
                                -> Satisfiable Constraints ExclusionReason
addTopLevelDependencyConstraint dep constraints =
  Constraints.constrain taggedDep reason constraints
  where
    taggedDep = TaggedDependency NoInstalledConstraint dep
    reason = ExcludedByTopLevelDependency dep

-- ------------------------------------------------------------
-- * Reasons for constraints
-- ------------------------------------------------------------

-- | For every constraint we record we also record the reason that constraint
-- is needed. So if we end up failing due to conflicting constraints then we
-- can give an explnanation as to what was conflicting and why.
--
data ExclusionReason =

     -- | We selected this other version of the package. That means we exclude
     -- all the other versions.
     SelectedOther PackageIdentifier

     -- | We excluded this version of the package because it failed to
     -- configure probably because of unsatisfiable deps.
   | ExcludedByConfigureFail

     -- | We excluded this version of the package because another package that
     -- we selected imposed a dependency which this package did not satisfy.
   | ExcludedByPackageDependency PackageIdentifier TaggedDependency

     -- | We excluded this version of the package because it did not satisfy
     -- a dependency given as an original top level input.
     --
   | ExcludedByTopLevelDependency Dependency

-- | Given an excluded package and the reason it was excluded, produce a human
-- readable explanation.
--
showExclusionReason :: PackageIdentifier -> ExclusionReason -> String
showExclusionReason pkgid (SelectedOther pkgid') =
  display pkgid ++ " was excluded because " ++
  display pkgid' ++ " was selected instead"
showExclusionReason pkgid ExcludedByConfigureFail =
  display pkgid ++ " was excluded because it could not be configured"
showExclusionReason pkgid (ExcludedByPackageDependency pkgid' dep) =
  display pkgid ++ " was excluded because " ++
  display pkgid' ++ " requires " ++ display (untagDependency dep)
showExclusionReason pkgid (ExcludedByTopLevelDependency dep) =
  display pkgid ++ " was excluded because of the top level dependency " ++
  display dep


-- ------------------------------------------------------------
-- * Logging progress and failures
-- ------------------------------------------------------------

data Log = Select SelectablePackage [SelectablePackage]
data Failure
   = ConfigureFailed
       SelectablePackage
       [(Dependency, [(PackageIdentifier, [ExclusionReason])])]
   | DependencyConflict
       SelectedPackage TaggedDependency
       [(PackageIdentifier, [ExclusionReason])]
   | TopLevelDependencyConflict
       Dependency
       [(PackageIdentifier, [ExclusionReason])]
   | TopLevelDependencyUnsatisfiable
       Dependency

showLog :: Log -> String
showLog (Select selected discarded) =
     "selecting " ++ displayPkg selected ++ " " ++ kind selected
  ++ case discarded of
       []  -> ""
       [d] -> " and discarding version " ++ display (packageVersion d)
       _   -> " and discarding versions "
           ++ listOf (display . packageVersion) discarded
  where
    kind (InstalledOnly _)           = "(installed)"
    kind (AvailableOnly _)           = "(hackage)"
    kind (InstalledAndAvailable _ _) = "(installed or hackage)"

showFailure :: Failure -> String
showFailure (ConfigureFailed pkg missingDeps) =
     "cannot configure " ++ displayPkg pkg ++ ". It requires "
  ++ listOf (display . fst) missingDeps
  ++ '\n' : unlines (map (uncurry whyNot) missingDeps)

  where
    whyNot (Dependency name ver) [] =
         "There is no available version of " ++ name
      ++ " that satisfies " ++ display ver

    whyNot dep conflicts =
         "For the dependency on " ++ display dep
      ++ " there are these packages: " ++ listOf display pkgs
      ++ ". However none of them are available.\n"
      ++ unlines [ showExclusionReason (packageId pkg') reason
                 | (pkg', reasons) <- conflicts, reason <- reasons ]

      where pkgs = map fst conflicts

showFailure (DependencyConflict pkg (TaggedDependency _ dep) conflicts) =
     "dependencies conflict: "
  ++ displayPkg pkg ++ " requires " ++ display dep ++ " however\n"
  ++ unlines [ showExclusionReason (packageId pkg') reason
             | (pkg', reasons) <- conflicts, reason <- reasons ]

showFailure (TopLevelDependencyConflict dep conflicts) =
     "dependencies conflict: "
  ++ "top level dependency " ++ display dep ++ " however\n"
  ++ unlines [ showExclusionReason (packageId pkg') reason
             | (pkg', reasons) <- conflicts, reason <- reasons ]

showFailure (TopLevelDependencyUnsatisfiable (Dependency name ver)) =
     "There is no available version of " ++ name
      ++ " that satisfies " ++ display ver

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

impossible :: a
impossible = internalError "impossible"

internalError :: String -> a
internalError msg = error $ "internal error: " ++ msg

displayPkg :: Package pkg => pkg -> String
displayPkg = display . packageId

listOf :: (a -> String) -> [a] -> String
listOf _    []   = []
listOf disp [x0] = disp x0
listOf disp (x0:x1:xs) = disp x0 ++ go x1 xs
  where go x []       = " and " ++ disp x
        go x (x':xs') = ", " ++ disp x ++ go x' xs'
