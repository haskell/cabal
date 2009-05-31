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
         ( AvailablePackage(..), ConfiguredPackage(..) )
import Distribution.Client.Dependency.Types
         ( DependencyResolver, PackageConstraint(..)
         , PackagePreferences(..), InstalledPreference(..)
         , Progress(..), foldProgress )

import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Package
         ( PackageName(..), PackageIdentifier, Package(packageId), packageVersion, packageName
         , Dependency(Dependency), thisPackageVersion, notThisPackageVersion
         , PackageFixedDeps(depends) )
import Distribution.PackageDescription
         ( PackageDescription(buildDepends) )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription, flattenPackageDescription )
import Distribution.Version
         ( VersionRange, anyVersion, withinRange, simplifyVersionRange )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( Platform(Platform) )
import Distribution.Simple.Utils
         ( equating, comparing )
import Distribution.Text
         ( display )

import Data.List
         ( foldl', maximumBy, minimumBy, nub, sort, groupBy )
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
      InstalledOnly           (InstalledPackage    _ i _) -> i
      AvailableOnly           (UnconfiguredPackage _ i _) -> i
      InstalledAndAvailable _ (UnconfiguredPackage _ i _) -> i

    bestByPref pkgname = case packageInstalledPreference of
        PreferLatest    ->
          comparing (\(p,_) -> (               isPreferred p, packageId p))
        PreferInstalled ->
          comparing (\(p,_) -> (isInstalled p, isPreferred p, packageId p))
      where
        isInstalled (AvailableOnly _) = False
        isInstalled _                 = True
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
type SelectionChanges = ([SelectedPackage], [PackageIdentifier])

searchSpace :: ConfigurePackage
            -> Constraints
            -> SelectedPackages
            -> SelectionChanges
            -> Set PackageName
            -> SearchSpace (SelectedPackages, Constraints, SelectionChanges)
                           SelectablePackage
searchSpace configure constraints selected changes next =
  ChoiceNode (selected, constraints, changes)
    [ [ (pkg, select name pkg)
      | pkg <- PackageIndex.lookupPackageName available name ]
    | name <- Set.elems next ]
  where
    available = Constraints.choices constraints

    select name pkg = case configure available pkg of
      Left missing -> Failure $ ConfigureFailed pkg
                        [ (dep, Constraints.conflicting constraints dep)
                        | dep <- missing ]
      Right pkg' -> case constrainDeps pkg' newDeps constraints [] of
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
                      | dep <- newDeps
                      , let (Dependency name' _) = untagDependency dep
                      , null (PackageIndex.lookupPackageName selected' name') ]
          newDeps   = concatMap packageConstraints newSelected
          next'     = Set.delete name
                    $ foldl' (flip Set.insert) next newPkgs

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
              -> [PackageIdentifier]
              -> Either Failure (Constraints, [PackageIdentifier])
constrainDeps pkg []         cs discard =
  case addPackageSelectConstraint (packageId pkg) cs of
    Satisfiable cs' discard' -> Right (cs', discard' ++ discard)
    _                        -> impossible
constrainDeps pkg (dep:deps) cs discard =
  case addPackageDependencyConstraint (packageId pkg) dep cs of
    Satisfiable cs' discard' -> constrainDeps pkg deps cs' (discard' ++ discard)
    Unsatisfiable            -> impossible
    ConflictsWith conflicts  ->
      Left (DependencyConflict pkg dep conflicts)

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
                 -> PackageIndex InstalledPackageInfo
                 -> PackageIndex AvailablePackage
                 -> (PackageName -> PackagePreferences)
                 -> [PackageConstraint]
                 -> [PackageName]
                 -> Progress Log Failure [PlanPackage]
topDownResolver' platform comp installed available
                 preferences constraints targets =
      fmap (uncurry finalise)
    . (\cs -> search configure preferences cs initialPkgNames)
  =<< addTopLevelConstraints constraints constraintSet

  where
    configure   = configurePackage platform comp
    constraintSet = Constraints.empty
      (annotateInstalledPackages             topSortNumber installed')
      (annotateAvailablePackages constraints topSortNumber available')
    (installed', available') = selectNeededSubset installed available
                                                  initialPkgNames
    topSortNumber = topologicalSortNumbering installed' available'

    initialPkgNames = Set.fromList targets

    finalise selected' constraints' =
        PackageIndex.allPackages
      . fst . improvePlan installed' constraints'
      . PackageIndex.fromList
      $ finaliseSelectedPackages preferences selected' constraints'

addTopLevelConstraints :: [PackageConstraint] -> Constraints
                       -> Progress a Failure Constraints
addTopLevelConstraints []                                      cs = Done cs
addTopLevelConstraints (PackageFlagsConstraint   _   _  :deps) cs =
  addTopLevelConstraints deps cs

addTopLevelConstraints (PackageVersionConstraint pkg ver:deps) cs =
  case addTopLevelVersionConstraint pkg ver cs of
    Satisfiable cs' _       ->
      addTopLevelConstraints deps cs'

    Unsatisfiable           ->
      Fail (TopLevelVersionConstraintUnsatisfiable pkg ver)

    ConflictsWith conflicts ->
      Fail (TopLevelVersionConstraintConflict pkg ver conflicts)

addTopLevelConstraints (PackageInstalledConstraint pkg:deps) cs =
  case addTopLevelInstalledConstraint pkg cs of
    Satisfiable cs' _       -> addTopLevelConstraints deps cs'

    Unsatisfiable           ->
      Fail (TopLevelInstallConstraintUnsatisfiable pkg)

    ConflictsWith conflicts ->
      Fail (TopLevelInstallConstraintConflict pkg conflicts)

configurePackage :: Platform -> CompilerId -> ConfigurePackage
configurePackage (Platform arch os) comp available spkg = case spkg of
  InstalledOnly         ipkg      -> Right (InstalledOnly ipkg)
  AvailableOnly              apkg -> fmap AvailableOnly (configure apkg)
  InstalledAndAvailable ipkg apkg -> fmap (InstalledAndAvailable ipkg)
                                          (configure apkg)
  where
  configure (UnconfiguredPackage apkg@(AvailablePackage _ p _) _ flags) =
    case finalizePackageDescription flags (Just available) os arch comp [] p of
      Left missing        -> Left missing
      Right (pkg, flags') -> Right $
        SemiConfiguredPackage apkg flags' (buildDepends pkg)

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
    transitiveDepends = map (packageId . toPkg) . tail . Graph.reachable graph
                      . fromJust . toVertex . packageId
    (graph, toPkg, toVertex) = PackageIndex.dependencyGraph installed


-- | Annotate each available packages with its topological sort number and any
-- user-supplied partial flag assignment.
--
annotateAvailablePackages :: [PackageConstraint]
                          -> (PackageName -> TopologicalSortNumber)
                          -> PackageIndex AvailablePackage
                          -> PackageIndex UnconfiguredPackage
annotateAvailablePackages constraints dfsNumber available = PackageIndex.fromList
  [ UnconfiguredPackage pkg (dfsNumber name) (flagsFor name)
  | pkg <- PackageIndex.allPackages available
  , let name = packageName pkg ]
  where
    flagsFor = fromMaybe [] . flip Map.lookup flagsMap
    flagsMap = Map.fromList
      [ (name, flags)
      | PackageFlagsConstraint name flags <- constraints ]

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

-- | We don't need the entire index (which is rather large and costly if we
-- force it by examining the whole thing). So trace out the maximul subset of
-- each index that we could possibly ever need. Do this by flattening packages
-- and looking at the names of all possible dependencies.
--
selectNeededSubset :: PackageIndex InstalledPackageInfo
                   -> PackageIndex AvailablePackage
                   -> Set PackageName
                   -> (PackageIndex InstalledPackageInfo
                      ,PackageIndex AvailablePackage)
selectNeededSubset installed available = select mempty mempty
  where
    select :: PackageIndex InstalledPackageInfo
           -> PackageIndex AvailablePackage
           -> Set PackageName
           -> (PackageIndex InstalledPackageInfo
              ,PackageIndex AvailablePackage)
    select installed' available' remaining
      | Set.null remaining = (installed', available')
      | otherwise = select installed'' available'' remaining''
      where
        (next, remaining') = Set.deleteFindMin remaining
        moreInstalled = PackageIndex.lookupPackageName installed next
        moreAvailable = PackageIndex.lookupPackageName available next
        moreRemaining = -- we filter out packages already included in the indexes
                        -- this avoids an infinite loop if a package depends on itself
                        -- like base-3.0.3.0 with base-4.0.0.0
                        filter notAlreadyIncluded
                      $ [ packageName dep
                        | pkg <- moreInstalled
                        , dep <- depends pkg ]
                     ++ [ name
                        | AvailablePackage _ pkg _ <- moreAvailable
                        , Dependency name _ <-
                            buildDepends (flattenPackageDescription pkg) ]
        installed''   = foldl' (flip PackageIndex.insert) installed' moreInstalled
        available''   = foldl' (flip PackageIndex.insert) available' moreAvailable
        remaining''   = foldl' (flip         Set.insert) remaining' moreRemaining
        notAlreadyIncluded name = null (PackageIndex.lookupPackageName installed' name)
                                  && null (PackageIndex.lookupPackageName available' name)

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
    finaliseSelected (InstalledOnly         ipkg     ) = finaliseInstalled ipkg
    finaliseSelected (AvailableOnly              apkg) = finaliseAvailable Nothing apkg
    finaliseSelected (InstalledAndAvailable ipkg apkg) =
      case PackageIndex.lookupPackageId remainingChoices (packageId ipkg) of
        Nothing                          -> impossible --picked package not in constraints
        Just (AvailableOnly _)           -> impossible --to constrain to avail only
        Just (InstalledOnly _)           -> finaliseInstalled ipkg
        Just (InstalledAndAvailable _ _) -> finaliseAvailable (Just ipkg) apkg

    finaliseInstalled (InstalledPackage pkg _ _) = InstallPlan.PreExisting pkg
    finaliseAvailable mipkg (SemiConfiguredPackage pkg flags deps) =
      InstallPlan.Configured (ConfiguredPackage pkg flags deps')
      where
        deps' = map (packageId . pickRemaining) deps
        pickRemaining dep =
          case PackageIndex.lookupDependency remainingChoices dep of
            []        -> impossible
            [pkg']    -> pkg'
            remaining -> assert (checkIsPaired remaining)
                       $ maximumBy bestByPref remaining
        -- We order candidate packages to pick for a dependency by these
        -- three factors. The last factor is just highest version wins.
        bestByPref =
          comparing (\p -> (isCurrent p, isPreferred p, packageVersion p))
        -- Is the package already used by the installed version of this
        -- package? If so we should pick that first. This stops us from doing
        -- silly things like deciding to rebuild haskell98 against base 3.
        isCurrent = case mipkg :: Maybe InstalledPackage of
          Nothing   -> \_ -> False
          Just ipkg -> \p -> packageId p `elem` depends ipkg
        -- Is this package a preferred version acording to the hackage or
        -- user's suggested version constraints
        isPreferred p = packageVersion p `withinRange` preferredVersions
          where (PackagePreferences preferredVersions _) = pref (packageName p)

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
improvePlan :: PackageIndex InstalledPackageInfo
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
                 -> [InstalledPackageInfo]
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
      case addPackageDependencyConstraint pkgid dep constraints of
        Satisfiable constraints' _ -> constraintsOk pkgid pkgids constraints'
        _                          -> Nothing
      where
        dep = TaggedDependency InstalledConstraint (thisPackageVersion pkgid')

    reverseTopologicalOrder :: PackageFixedDeps pkg
                            => PackageIndex pkg -> [PackageIdentifier]
    reverseTopologicalOrder index = map (packageId . toPkg)
                                  . Graph.topSort
                                  . Graph.transposeG
                                  $ graph
      where (graph, toPkg, _) = PackageIndex.dependencyGraph index

-- ------------------------------------------------------------
-- * Adding and recording constraints
-- ------------------------------------------------------------

addPackageSelectConstraint :: PackageIdentifier -> Constraints
                           -> Satisfiable Constraints
                                [PackageIdentifier] ExclusionReason
addPackageSelectConstraint pkgid constraints =
  Constraints.constrain dep reason constraints
  where
    dep    = TaggedDependency NoInstalledConstraint (thisPackageVersion pkgid)
    reason = SelectedOther pkgid

addPackageExcludeConstraint :: PackageIdentifier -> Constraints
                            -> Satisfiable Constraints
                                 [PackageIdentifier] ExclusionReason
addPackageExcludeConstraint pkgid constraints =
  Constraints.constrain dep reason constraints
  where
    dep    = TaggedDependency NoInstalledConstraint
               (notThisPackageVersion pkgid)
    reason = ExcludedByConfigureFail

addPackageDependencyConstraint :: PackageIdentifier -> TaggedDependency -> Constraints
                               -> Satisfiable Constraints
                                    [PackageIdentifier] ExclusionReason
addPackageDependencyConstraint pkgid dep constraints =
  Constraints.constrain dep reason constraints
  where
    reason = ExcludedByPackageDependency pkgid dep

addTopLevelVersionConstraint :: PackageName -> VersionRange
                             -> Constraints
                             -> Satisfiable Constraints
                                  [PackageIdentifier] ExclusionReason
addTopLevelVersionConstraint pkg ver constraints =
  Constraints.constrain taggedDep reason constraints
  where
    dep       = Dependency pkg ver
    taggedDep = TaggedDependency NoInstalledConstraint dep
    reason    = ExcludedByTopLevelDependency dep

addTopLevelInstalledConstraint :: PackageName
                               -> Constraints
                               -> Satisfiable Constraints
                                    [PackageIdentifier] ExclusionReason
addTopLevelInstalledConstraint pkg constraints =
  Constraints.constrain taggedDep reason constraints
  where
    dep       = Dependency pkg anyVersion
    taggedDep = TaggedDependency InstalledConstraint dep
    reason    = ExcludedByTopLevelDependency dep

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
  display pkgid' ++ " requires " ++ displayDep (untagDependency dep)
showExclusionReason pkgid (ExcludedByTopLevelDependency dep) =
  display pkgid ++ " was excluded because of the top level dependency " ++
  displayDep dep


-- ------------------------------------------------------------
-- * Logging progress and failures
-- ------------------------------------------------------------

data Log = Select [SelectedPackage] [PackageIdentifier]
data Failure
   = ConfigureFailed
       SelectablePackage
       [(Dependency, [(PackageIdentifier, [ExclusionReason])])]
   | DependencyConflict
       SelectedPackage TaggedDependency
       [(PackageIdentifier, [ExclusionReason])]
   | TopLevelVersionConstraintConflict
       PackageName VersionRange
       [(PackageIdentifier, [ExclusionReason])]
   | TopLevelVersionConstraintUnsatisfiable
       PackageName VersionRange
   | TopLevelInstallConstraintConflict
       PackageName
       [(PackageIdentifier, [ExclusionReason])]
   | TopLevelInstallConstraintUnsatisfiable
       PackageName

showLog :: Log -> String
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

    kind (InstalledOnly _)           = "(installed)"
    kind (AvailableOnly _)           = "(hackage)"
    kind (InstalledAndAvailable _ _) = "(installed or hackage)"

    discardedMsg = case discarded of
      []  -> ""
      _   -> "discarding " ++ listOf id
        [ element
        | (pkgid:pkgids) <- groupBy (equating packageName) (sort discarded)
        , element <- display pkgid : map (display . packageVersion) pkgids ]

showFailure :: Failure -> String
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

showFailure (DependencyConflict pkg (TaggedDependency _ dep) conflicts) =
     "dependencies conflict: "
  ++ displayPkg pkg ++ " requires " ++ displayDep dep ++ " however\n"
  ++ unlines [ showExclusionReason (packageId pkg') reason
             | (pkg', reasons) <- conflicts, reason <- reasons ]

showFailure (TopLevelVersionConstraintConflict name ver conflicts) =
     "constraints conflict: "
  ++ "top level constraint " ++ displayDep (Dependency name ver) ++ " however\n"
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

simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range) =
  Dependency name (simplifyVersionRange range)

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
