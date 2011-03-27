-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency.Types
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Common types for dependency resolution.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency.TopDown (
    topDownResolver
  ) where

import Distribution.Client.Dependency.TopDown.Types
import qualified Distribution.Client.Dependency.TopDown.Constraints as Constraints
import Distribution.Client.Dependency.TopDown.Constraints
         ( Satisfiable(..) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan
         ( PlanPackage(..) )
import Distribution.Client.Types
         ( SourcePackage(..), ConfiguredPackage(..), InstalledPackage(..) )
import Distribution.Client.Dependency.Types
         ( DependencyResolver, PackageConstraint(..)
         , PackagePreferences(..), InstalledPreference(..)
         , Progress(..), foldProgress )

import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Client.PackageIndex (PackageIndex)
import Distribution.Package
         ( PackageName(..), PackageId, Package(..), packageVersion, packageName
         , Dependency(Dependency), thisPackageVersion
         , simplifyDependency, PackageFixedDeps(depends) )
import Distribution.PackageDescription
         ( PackageDescription(buildDepends) )
import Distribution.Client.PackageUtils
         ( externalBuildDepends )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription, flattenPackageDescription )
import Distribution.Version
         ( VersionRange, anyVersion, withinRange, simplifyVersionRange
         , isAnyVersion
         , UpperBound(..), asVersionIntervals )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( Platform )
import Distribution.Simple.Utils
         ( equating, comparing )
import Distribution.Text
         ( display )

import Data.List
         ( foldl', maximumBy, minimumBy, nub, sort, sortBy, groupBy )
import Data.Maybe
         ( fromJust, fromMaybe, catMaybes )
import Data.Monoid
         ( Monoid(mempty) )
import Control.Monad
         ( guard )
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Graph as Graph
import qualified Data.Array as Array
import Control.Exception
         ( assert )

-- ------------------------------------------------------------
-- * Search state types
-- ------------------------------------------------------------

type Constraints  = Constraints.Constraints
                      InstalledPackageEx UnconfiguredPackage ExclusionReason
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

explore :: (PackageName -> PackagePreferences)
        -> SearchSpace (SelectedPackages, Constraints, SelectionChanges)
                       SelectablePackage
        -> Progress Log Failure (SelectedPackages, Constraints)

explore _    (Failure failure)       = Fail failure
explore _    (ChoiceNode (s,c,_) []) = Done (s,c)
explore pref (ChoiceNode _ choices)  =
  case [ choice | [choice] <- choices ] of
    ((_, node'):_) -> Step (logInfo node') (explore pref node')
    []             -> Step (logInfo node') (explore pref node')
      where
        choice     = minimumBy (comparing topSortNumber) choices
        pkgname    = packageName . fst . head $ choice
        (_, node') = maximumBy (bestByPref pkgname) choice
  where
    topSortNumber choice = case fst (head choice) of
      InstalledOnly        (InstalledPackageEx  _ i _) -> i
      SourceOnly           (UnconfiguredPackage _ i _) -> i
      InstalledAndSource _ (UnconfiguredPackage _ i _) -> i

    bestByPref pkgname = case packageInstalledPreference of
        PreferLatest    ->
          comparing (\(p,_) -> (               isPreferred p, packageId p))
        PreferInstalled ->
          comparing (\(p,_) -> (isInstalled p, isPreferred p, packageId p))
      where
        isInstalled (SourceOnly _) = False
        isInstalled _              = True
        isPreferred p = packageVersion p `withinRange` preferredVersions
        (PackagePreferences preferredVersions packageInstalledPreference)
          = pref pkgname

    logInfo node = Select selected discarded
      where (selected, discarded) = case node of
              Failure    _               -> ([], [])
              ChoiceNode (_,_,changes) _ -> changes

-- ------------------------------------------------------------
-- * Generate a search tree
-- ------------------------------------------------------------

type ConfigurePackage = PackageIndex SelectablePackage
                     -> SelectablePackage
                     -> Either [Dependency] SelectedPackage

-- | (packages selected, packages discarded)
type SelectionChanges = ([SelectedPackage], [PackageId])

searchSpace :: ConfigurePackage
            -> Constraints
            -> SelectedPackages
            -> SelectionChanges
            -> Set PackageName
            -> SearchSpace (SelectedPackages, Constraints, SelectionChanges)
                           SelectablePackage
searchSpace configure constraints selected changes next =
  assert (Set.null (selectedSet `Set.intersection` next)) $
  assert (selectedSet `Set.isSubsetOf` Constraints.packages constraints) $
  assert (next `Set.isSubsetOf` Constraints.packages constraints) $

  ChoiceNode (selected, constraints, changes)
    [ [ (pkg, select name pkg)
      | pkg <- PackageIndex.lookupPackageName available name ]
    | name <- Set.elems next ]
  where
    available = Constraints.choices constraints

    selectedSet = Set.fromList (map packageName (PackageIndex.allPackages selected))

    select name pkg = case configure available pkg of
      Left missing -> Failure $ ConfigureFailed pkg
                        [ (dep, Constraints.conflicting constraints dep)
                        | dep <- missing ]
      Right pkg' ->
        case constrainDeps pkg' newDeps (addDeps constraints newPkgs) [] of
          Left failure       -> Failure failure
          Right (constraints', newDiscarded) ->
            searchSpace configure
              constraints' selected' (newSelected, newDiscarded) next'
        where
          selected' = foldl' (flip PackageIndex.insert) selected newSelected
          newSelected =
            case Constraints.isPaired constraints (packageId pkg) of
              Nothing     -> [pkg']
              Just pkgid' -> [pkg', pkg'']
                where
                  Just pkg'' = fmap (\(InstalledOnly p) -> InstalledOnly p)
                    (PackageIndex.lookupPackageId available pkgid')

          newPkgs   = [ name'
                      | (Dependency name' _, _) <- newDeps
                      , null (PackageIndex.lookupPackageName selected' name') ]
          newDeps   = concatMap packageConstraints newSelected
          next'     = Set.delete name
                    $ foldl' (flip Set.insert) next newPkgs

packageConstraints :: SelectedPackage -> [(Dependency, InstalledConstraint)]
packageConstraints = either installedConstraints availableConstraints
                   . preferSource
  where
    preferSource (InstalledOnly        pkg) = Left pkg
    preferSource (SourceOnly           pkg) = Right pkg
    preferSource (InstalledAndSource _ pkg) = Right pkg
    installedConstraints (InstalledPackageEx    _ _ deps) =
      [ (thisPackageVersion dep, InstalledConstraint)
      | dep <- deps ]
    availableConstraints (SemiConfiguredPackage _ _ deps) =
      [ (dep, NoInstalledConstraint) | dep <- deps ]

addDeps :: Constraints -> [PackageName] -> Constraints
addDeps =
  foldr $ \pkgname cs ->
            case Constraints.addTarget pkgname cs of
              Satisfiable cs' () -> cs'
              _                  -> impossible

constrainDeps :: SelectedPackage -> [(Dependency, InstalledConstraint)] -> Constraints
              -> [PackageId]
              -> Either Failure (Constraints, [PackageId])
constrainDeps pkg []         cs discard =
  case addPackageSelectConstraint (packageId pkg) cs of
    Satisfiable cs' discard' -> Right (cs', discard' ++ discard)
    _                        -> impossible
constrainDeps pkg ((dep, installedConstraint):deps) cs discard =
  case addPackageDependencyConstraint (packageId pkg) dep installedConstraint cs of
    Satisfiable cs' discard' -> constrainDeps pkg deps cs' (discard' ++ discard)
    Unsatisfiable            -> impossible
    ConflictsWith conflicts  ->
      Left (DependencyConflict pkg dep installedConstraint conflicts)

-- ------------------------------------------------------------
-- * The main algorithm
-- ------------------------------------------------------------

search :: ConfigurePackage
       -> (PackageName -> PackagePreferences)
       -> Constraints
       -> Set PackageName
       -> Progress Log Failure (SelectedPackages, Constraints)
search configure pref constraints =
  explore pref . searchSpace configure constraints mempty ([], [])

-- ------------------------------------------------------------
-- * The top level resolver
-- ------------------------------------------------------------

-- | The main exported resolver, with string logging and failure types to fit
-- the standard 'DependencyResolver' interface.
--
topDownResolver :: DependencyResolver
topDownResolver = ((((((mapMessages .).).).).).) . topDownResolver'
  where
    mapMessages :: Progress Log Failure a -> Progress String String a
    mapMessages = foldProgress (Step . showLog) (Fail . showFailure) Done

-- | The native resolver with detailed structured logging and failure types.
--
topDownResolver' :: Platform -> CompilerId
                 -> PackageIndex InstalledPackage
                 -> PackageIndex SourcePackage
                 -> (PackageName -> PackagePreferences)
                 -> [PackageConstraint]
                 -> [PackageName]
                 -> Progress Log Failure [PlanPackage]
topDownResolver' platform comp installedPkgIndex sourcePkgIndex
                 preferences constraints targets =
      fmap (uncurry finalise)
    . (\cs -> search configure preferences cs initialPkgNames)
  =<< pruneBottomUp platform comp
  =<< addTopLevelConstraints constraints
  =<< addTopLevelTargets targets emptyConstraintSet

  where
    configure   = configurePackage platform comp
    emptyConstraintSet :: Constraints
    emptyConstraintSet = Constraints.empty
      (annotateInstalledPackages          topSortNumber installedPkgIndex')
      (annotateSourcePackages constraints topSortNumber sourcePkgIndex')
    (installedPkgIndex', sourcePkgIndex') =
      selectNeededSubset installedPkgIndex sourcePkgIndex initialPkgNames
    topSortNumber = topologicalSortNumbering installedPkgIndex' sourcePkgIndex'

    initialPkgNames = Set.fromList targets

    finalise selected' constraints' =
        PackageIndex.allPackages
      . fst . improvePlan installedPkgIndex' constraints'
      . PackageIndex.fromList
      $ finaliseSelectedPackages preferences selected' constraints'


addTopLevelTargets :: [PackageName]
                   -> Constraints
                   -> Progress a Failure Constraints
addTopLevelTargets []         cs = Done cs
addTopLevelTargets (pkg:pkgs) cs =
  case Constraints.addTarget pkg cs of
    Satisfiable cs' ()       -> addTopLevelTargets pkgs cs'
    Unsatisfiable            -> Fail (NoSuchPackage pkg)
    ConflictsWith _conflicts -> impossible


addTopLevelConstraints :: [PackageConstraint] -> Constraints
                       -> Progress Log Failure Constraints
addTopLevelConstraints []                                      cs = Done cs
addTopLevelConstraints (PackageConstraintFlags   _   _  :deps) cs =
  addTopLevelConstraints deps cs

addTopLevelConstraints (PackageConstraintVersion pkg ver:deps) cs =
  case addTopLevelVersionConstraint pkg ver cs of
    Satisfiable cs' pkgids  ->
      foldr (Step . Exclude) (addTopLevelConstraints deps cs') pkgids

    Unsatisfiable           ->
      Fail (TopLevelVersionConstraintUnsatisfiable pkg ver)

    ConflictsWith conflicts ->
      Fail (TopLevelVersionConstraintConflict pkg ver conflicts)

addTopLevelConstraints (PackageConstraintInstalled pkg:deps) cs =
  case addTopLevelInstalledConstraint pkg cs of
    Satisfiable cs' pkgids  ->
      foldr (Step . Exclude) (addTopLevelConstraints deps cs') pkgids

    Unsatisfiable           ->
      Fail (TopLevelInstallConstraintUnsatisfiable pkg)

    ConflictsWith conflicts ->
      Fail (TopLevelInstallConstraintConflict pkg conflicts)


-- | Add exclusion on available packages that cannot be configured.
--
pruneBottomUp :: Platform -> CompilerId
              -> Constraints -> Progress Log Failure Constraints
pruneBottomUp platform comp constraints =
    foldr prune Done (initialPackages constraints) constraints

  where
    prune pkgs rest cs = foldr addExcludeConstraint rest unconfigurable cs
      where
        unconfigurable =
          [ (pkg, missing) -- if necessary we could look up missing reasons
          | (Just pkg', pkg) <- zip (map getSourcePkg pkgs) pkgs
          , Left missing <- [configure cs pkg'] ]

    addExcludeConstraint (pkg, missing) rest cs =
      let reason = ExcludedByConfigureFail missing in
      case addPackageExcludeConstraint (packageId pkg) reason cs of
        Satisfiable cs' [pkgid]| packageId pkg == pkgid
                         -> Step (Exclude pkgid) (rest cs')
        Satisfiable _ _  -> impossible
        Unsatisfiable    -> impossible
        ConflictsWith _  -> Fail $ ConfigureFailed pkg
                              [ (dep, Constraints.conflicting cs dep)
                              | dep <- missing ]

    configure cs (UnconfiguredPackage (SourcePackage _ pkg _) _ flags) =
      finalizePackageDescription flags (dependencySatisfiable cs)
                                 platform comp [] pkg
    dependencySatisfiable cs =
      not . null . PackageIndex.lookupDependency (Constraints.choices cs)

    -- collect each group of packages (by name) in reverse topsort order
    initialPackages =
        reverse
      . sortBy (comparing (topSortNumber . head))
      . PackageIndex.allPackagesByName
      . Constraints.choices

    topSortNumber (InstalledOnly        (InstalledPackageEx  _ i _)) = i
    topSortNumber (SourceOnly           (UnconfiguredPackage _ i _)) = i
    topSortNumber (InstalledAndSource _ (UnconfiguredPackage _ i _)) = i

    getSourcePkg (InstalledOnly      _     ) = Nothing
    getSourcePkg (SourceOnly           spkg) = Just spkg
    getSourcePkg (InstalledAndSource _ spkg) = Just spkg


configurePackage :: Platform -> CompilerId -> ConfigurePackage
configurePackage platform comp available spkg = case spkg of
  InstalledOnly      ipkg      -> Right (InstalledOnly ipkg)
  SourceOnly              apkg -> fmap SourceOnly (configure apkg)
  InstalledAndSource ipkg apkg -> fmap (InstalledAndSource ipkg)
                                       (configure apkg)
  where
  configure (UnconfiguredPackage apkg@(SourcePackage _ p _) _ flags) =
    case finalizePackageDescription flags dependencySatisfiable
                                    platform comp [] p of
      Left missing        -> Left missing
      Right (pkg, flags') -> Right $
        SemiConfiguredPackage apkg flags' (externalBuildDepends pkg)

  dependencySatisfiable = not . null . PackageIndex.lookupDependency available

-- | Annotate each installed packages with its set of transative dependencies
-- and its topological sort number.
--
annotateInstalledPackages :: (PackageName -> TopologicalSortNumber)
                          -> PackageIndex InstalledPackage
                          -> PackageIndex InstalledPackageEx
annotateInstalledPackages dfsNumber installed = PackageIndex.fromList
  [ InstalledPackageEx pkg (dfsNumber (packageName pkg)) (transitiveDepends pkg)
  | pkg <- PackageIndex.allPackages installed ]
  where
    transitiveDepends :: InstalledPackage -> [PackageId]
    transitiveDepends = map (packageId . toPkg) . tail . Graph.reachable graph
                      . fromJust . toVertex . packageId
    (graph, toPkg, toVertex) = PackageIndex.dependencyGraph installed


-- | Annotate each available packages with its topological sort number and any
-- user-supplied partial flag assignment.
--
annotateSourcePackages :: [PackageConstraint]
                       -> (PackageName -> TopologicalSortNumber)
                       -> PackageIndex SourcePackage
                       -> PackageIndex UnconfiguredPackage
annotateSourcePackages constraints dfsNumber sourcePkgIndex =
    PackageIndex.fromList
      [ UnconfiguredPackage pkg (dfsNumber name) (flagsFor name)
      | pkg <- PackageIndex.allPackages sourcePkgIndex
      , let name = packageName pkg ]
  where
    flagsFor = fromMaybe [] . flip Map.lookup flagsMap
    flagsMap = Map.fromList
      [ (name, flags)
      | PackageConstraintFlags name flags <- constraints ]

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
-- installed and source package sets. We consider only dependencies between
-- named packages, not including versions and for not-yet-configured packages
-- we look at all the possible dependencies, not just those under any single
-- flag assignment. This means we can actually get impossible combinations of
-- edges and even cycles, but that doesn't really matter here, it's only a
-- heuristic.
--
topologicalSortNumbering :: PackageIndex InstalledPackage
                         -> PackageIndex SourcePackage
                         -> (PackageName -> TopologicalSortNumber)
topologicalSortNumbering installedPkgIndex sourcePkgIndex =
    \pkgname -> let Just vertex = toVertex pkgname
                 in topologicalSortNumbers Array.! vertex
  where
    topologicalSortNumbers = Array.array (Array.bounds graph)
                                         (zip (Graph.topSort graph) [0..])
    (graph, _, toVertex)   = Graph.graphFromEdges $
         [ ((), packageName pkg, nub deps)
         | pkgs@(pkg:_) <- PackageIndex.allPackagesByName installedPkgIndex
         , let deps = [ packageName dep
                      | pkg' <- pkgs
                      , dep  <- depends pkg' ] ]
      ++ [ ((), packageName pkg, nub deps)
         | pkgs@(pkg:_) <- PackageIndex.allPackagesByName sourcePkgIndex
         , let deps = [ depName
                      | SourcePackage _ pkg' _ <- pkgs
                      , Dependency depName _ <-
                          buildDepends (flattenPackageDescription pkg') ] ]

-- | We don't need the entire index (which is rather large and costly if we
-- force it by examining the whole thing). So trace out the maximul subset of
-- each index that we could possibly ever need. Do this by flattening packages
-- and looking at the names of all possible dependencies.
--
selectNeededSubset :: PackageIndex InstalledPackage
                   -> PackageIndex SourcePackage
                   -> Set PackageName
                   -> (PackageIndex InstalledPackage
                      ,PackageIndex SourcePackage)
selectNeededSubset installedPkgIndex sourcePkgIndex = select mempty mempty
  where
    select :: PackageIndex InstalledPackage
           -> PackageIndex SourcePackage
           -> Set PackageName
           -> (PackageIndex InstalledPackage
              ,PackageIndex SourcePackage)
    select installedPkgIndex' sourcePkgIndex' remaining
      | Set.null remaining = (installedPkgIndex', sourcePkgIndex')
      | otherwise = select installedPkgIndex'' sourcePkgIndex'' remaining''
      where
        (next, remaining') = Set.deleteFindMin remaining
        moreInstalled = PackageIndex.lookupPackageName installedPkgIndex next
        moreSource    = PackageIndex.lookupPackageName sourcePkgIndex next
        moreRemaining = -- we filter out packages already included in the indexes
                        -- this avoids an infinite loop if a package depends on itself
                        -- like base-3.0.3.0 with base-4.0.0.0
                        filter notAlreadyIncluded
                      $ [ packageName dep
                        | pkg <- moreInstalled
                        , dep <- depends pkg ]
                     ++ [ name
                        | SourcePackage _ pkg _ <- moreSource
                        , Dependency name _ <-
                            buildDepends (flattenPackageDescription pkg) ]
        installedPkgIndex'' = foldl' (flip PackageIndex.insert)
                                     installedPkgIndex' moreInstalled
        sourcePkgIndex''    = foldl' (flip PackageIndex.insert)
                                     sourcePkgIndex' moreSource
        remaining''         = foldl' (flip          Set.insert)
                                     remaining' moreRemaining
        notAlreadyIncluded name =
            null (PackageIndex.lookupPackageName installedPkgIndex' name)
         && null (PackageIndex.lookupPackageName sourcePkgIndex' name)

-- ------------------------------------------------------------
-- * Post processing the solution
-- ------------------------------------------------------------

finaliseSelectedPackages :: (PackageName -> PackagePreferences)
                         -> SelectedPackages
                         -> Constraints
                         -> [PlanPackage]
finaliseSelectedPackages pref selected constraints =
  map finaliseSelected (PackageIndex.allPackages selected)
  where
    remainingChoices = Constraints.choices constraints
    finaliseSelected (InstalledOnly      ipkg     ) = finaliseInstalled ipkg
    finaliseSelected (SourceOnly              apkg) = finaliseSource Nothing apkg
    finaliseSelected (InstalledAndSource ipkg apkg) =
      case PackageIndex.lookupPackageId remainingChoices (packageId ipkg) of
        Nothing                          -> impossible --picked package not in constraints
        Just (SourceOnly _)           -> impossible --to constrain to avail only
        Just (InstalledOnly _)        -> finaliseInstalled ipkg
        Just (InstalledAndSource _ _) -> finaliseSource (Just ipkg) apkg

    finaliseInstalled (InstalledPackageEx pkg _ _) = InstallPlan.PreExisting pkg
    finaliseSource mipkg (SemiConfiguredPackage pkg flags deps) =
      InstallPlan.Configured (ConfiguredPackage pkg flags deps')
      where
        deps' = map (packageId . pickRemaining mipkg) deps

    pickRemaining mipkg dep@(Dependency _name versionRange) =
          case PackageIndex.lookupDependency remainingChoices dep of
            []        -> impossible
            [pkg']    -> pkg'
            remaining -> assert (checkIsPaired remaining)
                       $ maximumBy bestByPref remaining
      where
        -- We order candidate packages to pick for a dependency by these
        -- three factors. The last factor is just highest version wins.
        bestByPref =
          comparing (\p -> (isCurrent p, isPreferred p, packageVersion p))
        -- Is the package already used by the installed version of this
        -- package? If so we should pick that first. This stops us from doing
        -- silly things like deciding to rebuild haskell98 against base 3.
        isCurrent = case mipkg :: Maybe InstalledPackageEx of
          Nothing   -> \_ -> False
          Just ipkg -> \p -> packageId p `elem` depends ipkg
        -- If there is no upper bound on the version range then we apply a
        -- preferred version according to the hackage or user's suggested
        -- version constraints. TODO: distinguish hacks from prefs
        bounded = boundedAbove versionRange
        isPreferred p
          | bounded   = True -- any constant will do
          | otherwise = packageVersion p `withinRange` preferredVersions
          where (PackagePreferences preferredVersions _) = pref (packageName p)

        boundedAbove :: VersionRange -> Bool
        boundedAbove vr = case asVersionIntervals vr of
          []        -> True -- this is the inconsistent version range.
          intervals -> case last intervals of
            (_,   UpperBound _ _) -> True
            (_, NoUpperBound    ) -> False

        -- We really only expect to find more than one choice remaining when
        -- we're finalising a dependency on a paired package.
        checkIsPaired [p1, p2] =
          case Constraints.isPaired constraints (packageId p1) of
            Just p2'   -> packageId p2' == packageId p2
            Nothing    -> False
        checkIsPaired _ = False

-- | Improve an existing installation plan by, where possible, swapping
-- packages we plan to install with ones that are already installed.
-- This may add additional constraints due to the dependencies of installed
-- packages on other installed packages.
--
improvePlan :: PackageIndex InstalledPackage
            -> Constraints
            -> PackageIndex PlanPackage
            -> (PackageIndex PlanPackage, Constraints)
improvePlan installed constraints0 selected0 =
  foldl' improve (selected0, constraints0) (reverseTopologicalOrder selected0)
  where
    improve (selected, constraints) = fromMaybe (selected, constraints)
                                    . improvePkg selected constraints

    -- The idea is to improve the plan by swapping a configured package for
    -- an equivalent installed one. For a particular package the condition is
    -- that the package be in a configured state, that a the same version be
    -- already installed with the exact same dependencies and all the packages
    -- in the plan that it depends on are in the installed state
    improvePkg selected constraints pkgid = do
      Configured pkg  <- PackageIndex.lookupPackageId selected  pkgid
      ipkg            <- PackageIndex.lookupPackageId installed pkgid
      guard $ all (isInstalled selected) (depends pkg)
      tryInstalled selected constraints [ipkg]

    isInstalled selected pkgid =
      case PackageIndex.lookupPackageId selected pkgid of
        Just (PreExisting _) -> True
        _                    -> False

    tryInstalled :: PackageIndex PlanPackage -> Constraints
                 -> [InstalledPackage]
                 -> Maybe (PackageIndex PlanPackage, Constraints)
    tryInstalled selected constraints [] = Just (selected, constraints)
    tryInstalled selected constraints (pkg:pkgs) =
      case constraintsOk (packageId pkg) (depends pkg) constraints of
        Nothing           -> Nothing
        Just constraints' -> tryInstalled selected' constraints' pkgs'
          where
            selected' = PackageIndex.insert (PreExisting pkg) selected
            pkgs'      = catMaybes (map notSelected (depends pkg)) ++ pkgs
            notSelected pkgid =
              case (PackageIndex.lookupPackageId installed pkgid
                   ,PackageIndex.lookupPackageId selected  pkgid) of
                (Just pkg', Nothing) -> Just pkg'
                _                    -> Nothing

    constraintsOk _     []              constraints = Just constraints
    constraintsOk pkgid (pkgid':pkgids) constraints =
      case addPackageDependencyConstraint
             pkgid dep InstalledConstraint constraints of
        Satisfiable constraints' _ -> constraintsOk pkgid pkgids constraints'
        _                          -> Nothing
      where
        dep = thisPackageVersion pkgid'

    reverseTopologicalOrder :: PackageFixedDeps pkg
                            => PackageIndex pkg -> [PackageId]
    reverseTopologicalOrder index = map (packageId . toPkg)
                                  . Graph.topSort
                                  . Graph.transposeG
                                  $ graph
      where (graph, toPkg, _) = PackageIndex.dependencyGraph index

-- ------------------------------------------------------------
-- * Adding and recording constraints
-- ------------------------------------------------------------

addPackageSelectConstraint :: PackageId -> Constraints
                           -> Satisfiable Constraints
                                [PackageId] ExclusionReason
addPackageSelectConstraint pkgid =
    Constraints.constrain pkgname constraint reason
  where
    pkgname          = packageName pkgid
    constraint ver _ = ver == packageVersion pkgid
    reason           = SelectedOther pkgid

addPackageExcludeConstraint :: PackageId -> ExclusionReason
                            -> Constraints
                            -> Satisfiable Constraints
                                           [PackageId] ExclusionReason
addPackageExcludeConstraint pkgid reason =
    Constraints.constrain pkgname constraint reason
  where
    pkgname = packageName pkgid
    constraint ver installed
      | ver == packageVersion pkgid = installed
      | otherwise                   = True

addPackageDependencyConstraint :: PackageId -> Dependency -> InstalledConstraint
                               -> Constraints
                               -> Satisfiable Constraints
                                    [PackageId] ExclusionReason
addPackageDependencyConstraint pkgid dep@(Dependency pkgname verrange)
                                     installedConstraint =
    Constraints.constrain pkgname constraint reason
  where
    constraint ver installed = ver `withinRange` verrange
                            && case installedConstraint of
                                 InstalledConstraint   -> installed
                                 NoInstalledConstraint -> True
    reason = ExcludedByPackageDependency pkgid dep installedConstraint

addTopLevelVersionConstraint :: PackageName -> VersionRange
                             -> Constraints
                             -> Satisfiable Constraints
                                  [PackageId] ExclusionReason
addTopLevelVersionConstraint pkgname verrange =
    Constraints.constrain pkgname constraint reason
  where
    constraint ver _installed = ver `withinRange` verrange
    reason = ExcludedByTopLevelDependency (Dependency pkgname verrange)
                                          NoInstalledConstraint

addTopLevelInstalledConstraint :: PackageName
                               -> Constraints
                               -> Satisfiable Constraints
                                    [PackageId] ExclusionReason
addTopLevelInstalledConstraint pkgname =
    Constraints.constrain pkgname constraint reason
  where
    constraint _ver installed = installed
    reason = ExcludedByTopLevelDependency (Dependency pkgname anyVersion)
                                          InstalledConstraint

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
     SelectedOther PackageId

     -- | We excluded this version of the package because it failed to
     -- configure probably because of unsatisfiable deps.
   | ExcludedByConfigureFail [Dependency]

     -- | We excluded this version of the package because another package that
     -- we selected imposed a dependency which this package did not satisfy.
   | ExcludedByPackageDependency PackageId Dependency InstalledConstraint

     -- | We excluded this version of the package because it did not satisfy
     -- a dependency given as an original top level input.
     --
   | ExcludedByTopLevelDependency Dependency InstalledConstraint
  deriving Eq

-- | Given an excluded package and the reason it was excluded, produce a human
-- readable explanation.
--
showExclusionReason :: PackageId -> ExclusionReason -> String
showExclusionReason pkgid (SelectedOther pkgid') =
  display pkgid ++ " was excluded because " ++
  display pkgid' ++ " was selected instead"
showExclusionReason pkgid (ExcludedByConfigureFail missingDeps) =
  display pkgid ++ " was excluded because it could not be configured. "
  ++ "It requires " ++ listOf displayDep missingDeps
showExclusionReason pkgid (ExcludedByPackageDependency pkgid' dep _) =
  display pkgid ++ " was excluded because " ++
  display pkgid' ++ " requires " ++ displayDep dep
showExclusionReason pkgid (ExcludedByTopLevelDependency
                            (Dependency pkgname verRange) InstalledConstraint)
  | isAnyVersion verRange
  = display pkgid ++ " was excluded because only installed instances of "
 ++ display pkgname ++ " can be selected."

showExclusionReason pkgid (ExcludedByTopLevelDependency dep _) =
  display pkgid ++ " was excluded because of the top level constraint " ++
  displayDep dep


-- ------------------------------------------------------------
-- * Logging progress and failures
-- ------------------------------------------------------------

data Log = Select [SelectedPackage] [PackageId]
         | Exclude PackageId
data Failure
   = NoSuchPackage
       PackageName
   | ConfigureFailed
       SelectablePackage
       [(Dependency, [(PackageId, [ExclusionReason])])]
   | DependencyConflict
       SelectedPackage Dependency InstalledConstraint
       [(PackageId, [ExclusionReason])]
   | TopLevelVersionConstraintConflict
       PackageName VersionRange
       [(PackageId, [ExclusionReason])]
   | TopLevelVersionConstraintUnsatisfiable
       PackageName VersionRange
   | TopLevelInstallConstraintConflict
       PackageName
       [(PackageId, [ExclusionReason])]
   | TopLevelInstallConstraintUnsatisfiable
       PackageName

showLog :: Log -> String
showLog (Exclude excluded) = "excluding " ++ display excluded
showLog (Select selected discarded) = case (selectedMsg, discardedMsg) of
  ("", y) -> y
  (x, "") -> x
  (x,  y) -> x ++ " and " ++ y

  where
    selectedMsg  = "selecting " ++ case selected of
      []     -> ""
      [s]    -> display (packageId s) ++ " " ++ kind s
      (s:ss) -> listOf id
              $ (display (packageId s) ++ " " ++ kind s)
              : [ display (packageVersion s') ++ " " ++ kind s'
                | s' <- ss ]

    kind (InstalledOnly _)        = "(installed)"
    kind (SourceOnly _)           = "(source)"
    kind (InstalledAndSource _ _) = "(installed or source)"

    discardedMsg = case discarded of
      []  -> ""
      _   -> "discarding " ++ listOf id
        [ element
        | (pkgid:pkgids) <- groupBy (equating packageName) (sort discarded)
        , element <- display pkgid : map (display . packageVersion) pkgids ]

showFailure :: Failure -> String
showFailure (NoSuchPackage pkgname) =
     "The package " ++ display pkgname ++ " is unknown."
showFailure (ConfigureFailed pkg missingDeps) =
     "cannot configure " ++ displayPkg pkg ++ ". It requires "
  ++ listOf (displayDep . fst) missingDeps
  ++ '\n' : unlines (map (uncurry whyNot) missingDeps)

  where
    whyNot (Dependency name ver) [] =
         "There is no available version of " ++ display name
      ++ " that satisfies " ++ displayVer ver

    whyNot dep conflicts =
         "For the dependency on " ++ displayDep dep
      ++ " there are these packages: " ++ listOf display pkgs
      ++ ". However none of them are available.\n"
      ++ unlines [ showExclusionReason (packageId pkg') reason
                 | (pkg', reasons) <- conflicts, reason <- reasons ]

      where pkgs = map fst conflicts

showFailure (DependencyConflict pkg dep _ conflicts) =
     "dependencies conflict: "
  ++ displayPkg pkg ++ " requires " ++ displayDep dep ++ " however\n"
  ++ unlines [ showExclusionReason (packageId pkg') reason
             | (pkg', reasons) <- conflicts, reason <- reasons ]

showFailure (TopLevelVersionConstraintConflict name ver conflicts) =
     "constraints conflict: we have the top level constraint "
  ++ displayDep (Dependency name ver) ++ ", but\n"
  ++ unlines [ showExclusionReason (packageId pkg') reason
             | (pkg', reasons) <- conflicts, reason <- reasons ]

showFailure (TopLevelVersionConstraintUnsatisfiable name ver) =
     "There is no available version of " ++ display name
      ++ " that satisfies " ++ displayVer ver

showFailure (TopLevelInstallConstraintConflict name conflicts) =
     "constraints conflict: "
  ++ "top level constraint " ++ display name ++ "-installed however\n"
  ++ unlines [ showExclusionReason (packageId pkg') reason
             | (pkg', reasons) <- conflicts, reason <- reasons ]

showFailure (TopLevelInstallConstraintUnsatisfiable name) =
     "There is no installed version of " ++ display name

displayVer :: VersionRange -> String
displayVer = display . simplifyVersionRange

displayDep :: Dependency -> String
displayDep = display . simplifyDependency


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
