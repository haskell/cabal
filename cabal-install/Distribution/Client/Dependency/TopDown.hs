{-# LANGUAGE CPP #-}
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
import Distribution.Client.Types
         ( SourcePackage(..), ConfiguredPackage(..)
         , enableStanzas, ConfiguredId(..), fakeInstalledPackageId )
import Distribution.Client.Dependency.Types
         ( DependencyResolver, ResolverPackage(..)
         , PackageConstraint(..), unlabelPackageConstraint
         , PackagePreferences(..), InstalledPreference(..)
         , Progress(..), foldProgress )

import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Simple.PackageIndex  as InstalledPackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Client.ComponentDeps
         ( ComponentDeps )
import qualified Distribution.Client.ComponentDeps as CD
import Distribution.Client.PackageIndex
         ( PackageIndex )
import Distribution.Package
         ( PackageName(..), PackageId, PackageIdentifier(..)
         , InstalledPackageId(..)
         , Package(..), packageVersion, packageName
         , Dependency(Dependency), thisPackageVersion, simplifyDependency )
import Distribution.PackageDescription
         ( PackageDescription(buildDepends) )
import Distribution.Client.PackageUtils
         ( externalBuildDepends )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription, flattenPackageDescription )
import Distribution.Version
         ( Version(..), VersionRange, withinRange, simplifyVersionRange
         , UpperBound(..), asVersionIntervals )
import Distribution.Compiler
         ( CompilerInfo )
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
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
         ( Monoid(mempty) )
#endif
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
      SourceOnly           (UnconfiguredPackage _ i _ _) -> i
      InstalledAndSource _ (UnconfiguredPackage _ i _ _) -> i

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

packageConstraints :: SelectedPackage -> [(Dependency, Bool)]
packageConstraints = either installedConstraints availableConstraints
                   . preferSource
  where
    preferSource (InstalledOnly        pkg) = Left pkg
    preferSource (SourceOnly           pkg) = Right pkg
    preferSource (InstalledAndSource _ pkg) = Right pkg
    installedConstraints (InstalledPackageEx    _ _ deps) =
      [ (thisPackageVersion dep, True)
      | dep <- deps ]
    availableConstraints (SemiConfiguredPackage _ _ _ deps) =
      [ (dep, False) | dep <- deps ]

addDeps :: Constraints -> [PackageName] -> Constraints
addDeps =
  foldr $ \pkgname cs ->
            case Constraints.addTarget pkgname cs of
              Satisfiable cs' () -> cs'
              _                  -> impossible "addDeps unsatisfiable"

constrainDeps :: SelectedPackage -> [(Dependency, Bool)] -> Constraints
              -> [PackageId]
              -> Either Failure (Constraints, [PackageId])
constrainDeps pkg []         cs discard =
  case addPackageSelectConstraint (packageId pkg) cs of
    Satisfiable cs' discard' -> Right (cs', discard' ++ discard)
    _                        -> impossible "constrainDeps unsatisfiable(1)"
constrainDeps pkg ((dep, installedConstraint):deps) cs discard =
  case addPackageDependencyConstraint (packageId pkg) dep installedConstraint cs of
    Satisfiable cs' discard' -> constrainDeps pkg deps cs' (discard' ++ discard)
    Unsatisfiable            -> impossible "constrainDeps unsatisfiable(2)"
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
topDownResolver platform cinfo installedPkgIndex sourcePkgIndex
                preferences constraints targets =
    mapMessages $ topDownResolver'
                    platform cinfo
                    (convertInstalledPackageIndex installedPkgIndex)
                    sourcePkgIndex
                    preferences
                    (map unlabelPackageConstraint constraints)
                    targets
  where
    mapMessages :: Progress Log Failure a -> Progress String String a
    mapMessages = foldProgress (Step . showLog) (Fail . showFailure) Done

-- | The native resolver with detailed structured logging and failure types.
--
topDownResolver' :: Platform -> CompilerInfo
                 -> PackageIndex InstalledPackage
                 -> PackageIndex SourcePackage
                 -> (PackageName -> PackagePreferences)
                 -> [PackageConstraint]
                 -> [PackageName]
                 -> Progress Log Failure [ResolverPackage]
topDownResolver' platform cinfo installedPkgIndex sourcePkgIndex
                 preferences constraints targets =
      fmap (uncurry finalise)
    . (\cs -> search configure preferences cs initialPkgNames)
  =<< pruneBottomUp platform cinfo
  =<< addTopLevelConstraints constraints
  =<< addTopLevelTargets targets emptyConstraintSet

  where
    configure   = configurePackage platform cinfo
    emptyConstraintSet :: Constraints
    emptyConstraintSet = Constraints.empty
      (annotateInstalledPackages          topSortNumber installedPkgIndex')
      (annotateSourcePackages constraints topSortNumber sourcePkgIndex')
    (installedPkgIndex', sourcePkgIndex') =
      selectNeededSubset installedPkgIndex sourcePkgIndex initialPkgNames
    topSortNumber = topologicalSortNumbering installedPkgIndex' sourcePkgIndex'

    initialPkgNames = Set.fromList targets

    finalise selected' constraints' =
        map toResolverPackage
      . PackageIndex.allPackages
      . fst . improvePlan installedPkgIndex' constraints'
      . PackageIndex.fromList
      $ finaliseSelectedPackages preferences selected' constraints'

    toResolverPackage :: FinalSelectedPackage -> ResolverPackage
    toResolverPackage (SelectedInstalled (InstalledPackage pkg _))
                                              = PreExisting pkg
    toResolverPackage (SelectedSource    pkg) = Configured  pkg

addTopLevelTargets :: [PackageName]
                   -> Constraints
                   -> Progress a Failure Constraints
addTopLevelTargets []         cs = Done cs
addTopLevelTargets (pkg:pkgs) cs =
  case Constraints.addTarget pkg cs of
    Satisfiable cs' ()       -> addTopLevelTargets pkgs cs'
    Unsatisfiable            -> Fail (NoSuchPackage pkg)
    ConflictsWith _conflicts -> impossible "addTopLevelTargets conflicts"


addTopLevelConstraints :: [PackageConstraint] -> Constraints
                       -> Progress Log Failure Constraints
addTopLevelConstraints []                                      cs = Done cs
addTopLevelConstraints (PackageConstraintFlags   _   _  :deps) cs =
  addTopLevelConstraints deps cs

addTopLevelConstraints (PackageConstraintVersion pkg ver:deps) cs =
  case addTopLevelVersionConstraint pkg ver cs of
    Satisfiable cs' pkgids  ->
      Step (AppliedVersionConstraint pkg ver pkgids)
           (addTopLevelConstraints deps cs')

    Unsatisfiable           ->
      Fail (TopLevelVersionConstraintUnsatisfiable pkg ver)

    ConflictsWith conflicts ->
      Fail (TopLevelVersionConstraintConflict pkg ver conflicts)

addTopLevelConstraints (PackageConstraintInstalled pkg:deps) cs =
  case addTopLevelInstalledConstraint pkg cs of
    Satisfiable cs' pkgids  ->
      Step (AppliedInstalledConstraint pkg InstalledConstraint pkgids)
           (addTopLevelConstraints deps cs')

    Unsatisfiable           ->
      Fail (TopLevelInstallConstraintUnsatisfiable pkg InstalledConstraint)

    ConflictsWith conflicts ->
      Fail (TopLevelInstallConstraintConflict pkg InstalledConstraint conflicts)

addTopLevelConstraints (PackageConstraintSource pkg:deps) cs =
  case addTopLevelSourceConstraint pkg cs of
    Satisfiable cs' pkgids  ->
      Step (AppliedInstalledConstraint pkg SourceConstraint pkgids)
            (addTopLevelConstraints deps cs')

    Unsatisfiable           ->
      Fail (TopLevelInstallConstraintUnsatisfiable pkg SourceConstraint)

    ConflictsWith conflicts ->
      Fail (TopLevelInstallConstraintConflict pkg SourceConstraint conflicts)

addTopLevelConstraints (PackageConstraintStanzas _ _ : deps) cs =
    addTopLevelConstraints deps cs

-- | Add exclusion on available packages that cannot be configured.
--
pruneBottomUp :: Platform -> CompilerInfo
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
                         -> Step (ExcludeUnconfigurable pkgid) (rest cs')
        Satisfiable _ _  -> impossible "pruneBottomUp satisfiable"
        _                -> Fail $ ConfigureFailed pkg
                              [ (dep, Constraints.conflicting cs dep)
                              | dep <- missing ]

    configure cs (UnconfiguredPackage (SourcePackage _ pkg _ _) _ flags stanzas) =
      finalizePackageDescription flags (dependencySatisfiable cs)
                                 platform comp [] (enableStanzas stanzas pkg)
    dependencySatisfiable cs =
      not . null . PackageIndex.lookupDependency (Constraints.choices cs)

    -- collect each group of packages (by name) in reverse topsort order
    initialPackages =
        reverse
      . sortBy (comparing (topSortNumber . head))
      . PackageIndex.allPackagesByName
      . Constraints.choices

    topSortNumber (InstalledOnly        (InstalledPackageEx  _ i _)) = i
    topSortNumber (SourceOnly           (UnconfiguredPackage _ i _ _)) = i
    topSortNumber (InstalledAndSource _ (UnconfiguredPackage _ i _ _)) = i

    getSourcePkg (InstalledOnly      _     ) = Nothing
    getSourcePkg (SourceOnly           spkg) = Just spkg
    getSourcePkg (InstalledAndSource _ spkg) = Just spkg


configurePackage :: Platform -> CompilerInfo -> ConfigurePackage
configurePackage platform cinfo available spkg = case spkg of
  InstalledOnly      ipkg      -> Right (InstalledOnly ipkg)
  SourceOnly              apkg -> fmap SourceOnly (configure apkg)
  InstalledAndSource ipkg apkg -> fmap (InstalledAndSource ipkg)
                                       (configure apkg)
  where
  configure (UnconfiguredPackage apkg@(SourcePackage _ p _ _) _ flags stanzas) =
    case finalizePackageDescription flags dependencySatisfiable
                                    platform cinfo []
                                    (enableStanzas stanzas p) of
      Left missing        -> Left missing
      Right (pkg, flags') -> Right $
        SemiConfiguredPackage apkg flags' stanzas (externalBuildDepends pkg)

  dependencySatisfiable = not . null . PackageIndex.lookupDependency available

-- | Annotate each installed packages with its set of transitive dependencies
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
    (graph, toPkg, toVertex) = dependencyGraph installed


-- | Annotate each available packages with its topological sort number and any
-- user-supplied partial flag assignment.
--
annotateSourcePackages :: [PackageConstraint]
                       -> (PackageName -> TopologicalSortNumber)
                       -> PackageIndex SourcePackage
                       -> PackageIndex UnconfiguredPackage
annotateSourcePackages constraints dfsNumber sourcePkgIndex =
    PackageIndex.fromList
      [ UnconfiguredPackage pkg (dfsNumber name) (flagsFor name) (stanzasFor name)
      | pkg <- PackageIndex.allPackages sourcePkgIndex
      , let name = packageName pkg ]
  where
    flagsFor = fromMaybe [] . flip Map.lookup flagsMap
    flagsMap = Map.fromList
      [ (name, flags)
      | PackageConstraintFlags name flags <- constraints ]
    stanzasFor = fromMaybe [] . flip Map.lookup stanzasMap
    stanzasMap = Map.fromListWith (++)
        [ (name, stanzas)
        | PackageConstraintStanzas name stanzas <- constraints ]

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
                      , dep  <- sourceDeps pkg' ] ]
      ++ [ ((), packageName pkg, nub deps)
         | pkgs@(pkg:_) <- PackageIndex.allPackagesByName sourcePkgIndex
         , let deps = [ depName
                      | SourcePackage _ pkg' _ _ <- pkgs
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
                        , dep <- sourceDeps pkg ]
                     ++ [ name
                        | SourcePackage _ pkg _ _ <- moreSource
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


-- | The old top down solver assumes that installed packages are indexed by
-- their source package id. But these days they're actually indexed by an
-- installed package id and there can be many installed packages with the same
-- source package id. This function tries to do a convertion, but it can only
-- be partial.
--
convertInstalledPackageIndex :: InstalledPackageIndex
                             -> PackageIndex InstalledPackage
convertInstalledPackageIndex index' = PackageIndex.fromList
    -- There can be multiple installed instances of each package version,
    -- like when the same package is installed in the global & user DBs.
    -- InstalledPackageIndex.allPackagesBySourcePackageId gives us the
    -- installed packages with the most preferred instances first, so by
    -- picking the first we should get the user one. This is almost but not
    -- quite the same as what ghc does.
    [ InstalledPackage ipkg (sourceDepsOf index' ipkg)
    | (_,ipkg:_) <- InstalledPackageIndex.allPackagesBySourcePackageId index' ]
  where
    -- The InstalledPackageInfo only lists dependencies by the
    -- InstalledPackageId, which means we do not directly know the corresponding
    -- source dependency. The only way to find out is to lookup the
    -- InstalledPackageId to get the InstalledPackageInfo and look at its
    -- source PackageId. But if the package is broken because it depends on
    -- other packages that do not exist then we have a problem we cannot find
    -- the original source package id. Instead we make up a bogus package id.
    -- This should have the same effect since it should be a dependency on a
    -- nonexistent package.
    sourceDepsOf index ipkg =
      [ maybe (brokenPackageId depid) packageId mdep
      | let depids = InstalledPackageInfo.depends ipkg
            getpkg = InstalledPackageIndex.lookupInstalledPackageId index
      , (depid, mdep) <- zip depids (map getpkg depids) ]

    brokenPackageId (InstalledPackageId str) =
      PackageIdentifier (PackageName (str ++ "-broken")) (Version [] [])

-- ------------------------------------------------------------
-- * Post processing the solution
-- ------------------------------------------------------------

finaliseSelectedPackages :: (PackageName -> PackagePreferences)
                         -> SelectedPackages
                         -> Constraints
                         -> [FinalSelectedPackage]
finaliseSelectedPackages pref selected constraints =
  map finaliseSelected (PackageIndex.allPackages selected)
  where
    remainingChoices = Constraints.choices constraints
    finaliseSelected (InstalledOnly      ipkg     ) = finaliseInstalled ipkg
    finaliseSelected (SourceOnly              apkg) = finaliseSource Nothing apkg
    finaliseSelected (InstalledAndSource ipkg apkg) =
      case PackageIndex.lookupPackageId remainingChoices (packageId ipkg) of
                                        --picked package not in constraints
        Nothing                       -> impossible "finaliseSelected no pkg"
                                        -- to constrain to avail only:
        Just (SourceOnly _)           -> impossible "finaliseSelected src only"
        Just (InstalledOnly _)        -> finaliseInstalled ipkg
        Just (InstalledAndSource _ _) -> finaliseSource (Just ipkg) apkg

    finaliseInstalled (InstalledPackageEx pkg _ _) = SelectedInstalled pkg
    finaliseSource mipkg (SemiConfiguredPackage pkg flags stanzas deps) =
        SelectedSource (ConfiguredPackage pkg flags stanzas deps')
      where
        -- We cheat in the cabal solver, and classify all dependencies as
        -- library dependencies.
        deps' :: ComponentDeps [ConfiguredId]
        deps' = CD.fromLibraryDeps $ map (confId . pickRemaining mipkg) deps

    -- InstalledOrSource indicates that we either have a source package
    -- available, or an installed one, or both. In the case that we have both
    -- available, we don't yet know if we can pick the installed one (the
    -- dependencies may not match up, for instance); this is verified in
    -- `improvePlan`.
    --
    -- This means that at this point we cannot construct a valid installed
    -- package ID yet for the dependencies. We therefore have two options:
    --
    -- * We could leave the installed package ID undefined here, and have a
    --   separate pass over the output of the top-down solver, fixing all
    --   dependencies so that if we depend on an already installed package we
    --   use the proper installed package ID.
    --
    -- * We can _always_ use fake installed IDs, irrespective of whether we the
    --   dependency is on an already installed package or not. This is okay
    --   because (i) the top-down solver does not (and never will) support
    --   multiple package instances, and (ii) we initialize the FakeMap with
    --   fake IDs for already installed packages.
    --
    -- For now we use the second option; if however we change the implementation
    -- of these fake IDs so that we do away with the FakeMap and update a
    -- package reverse dependencies as we execute the install plan and discover
    -- real package IDs, then this is no longer possible and we have to
    -- implement the first option (see also Note [FakeMap] in Cabal).
    confId :: InstalledOrSource InstalledPackageEx UnconfiguredPackage -> ConfiguredId
    confId pkg = ConfiguredId {
        confSrcId  = packageId pkg
      , confInstId = fakeInstalledPackageId (packageId pkg)
      }

    pickRemaining mipkg dep@(Dependency _name versionRange) =
          case PackageIndex.lookupDependency remainingChoices dep of
            []        -> impossible "pickRemaining no pkg"
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
          Just ipkg -> \p -> packageId p `elem` sourceDeps ipkg
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
            -> PackageIndex FinalSelectedPackage
            -> (PackageIndex FinalSelectedPackage, Constraints)
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
      SelectedSource pkg  <- PackageIndex.lookupPackageId selected  pkgid
      ipkg                <- PackageIndex.lookupPackageId installed pkgid
      guard $ all (isInstalled selected) (sourceDeps pkg)
      tryInstalled selected constraints [ipkg]

    isInstalled selected pkgid =
      case PackageIndex.lookupPackageId selected pkgid of
        Just (SelectedInstalled _) -> True
        _                          -> False

    tryInstalled :: PackageIndex FinalSelectedPackage -> Constraints
                 -> [InstalledPackage]
                 -> Maybe (PackageIndex FinalSelectedPackage, Constraints)
    tryInstalled selected constraints [] = Just (selected, constraints)
    tryInstalled selected constraints (pkg:pkgs) =
      case constraintsOk (packageId pkg) (sourceDeps pkg) constraints of
        Nothing           -> Nothing
        Just constraints' -> tryInstalled selected' constraints' pkgs'
          where
            selected' = PackageIndex.insert (SelectedInstalled pkg) selected
            pkgs'      = catMaybes (map notSelected (sourceDeps pkg)) ++ pkgs
            notSelected pkgid =
              case (PackageIndex.lookupPackageId installed pkgid
                   ,PackageIndex.lookupPackageId selected  pkgid) of
                (Just pkg', Nothing) -> Just pkg'
                _                    -> Nothing

    constraintsOk _     []              constraints = Just constraints
    constraintsOk pkgid (pkgid':pkgids) constraints =
      case addPackageDependencyConstraint pkgid dep True constraints of
        Satisfiable constraints' _ -> constraintsOk pkgid pkgids constraints'
        _                          -> Nothing
      where
        dep = thisPackageVersion pkgid'

    reverseTopologicalOrder :: PackageIndex FinalSelectedPackage -> [PackageId]
    reverseTopologicalOrder index = map (packageId . toPkg)
                                  . Graph.topSort
                                  . Graph.transposeG
                                  $ graph
      where (graph, toPkg, _) = dependencyGraph index

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

addPackageDependencyConstraint :: PackageId -> Dependency -> Bool
                               -> Constraints
                               -> Satisfiable Constraints
                                    [PackageId] ExclusionReason
addPackageDependencyConstraint pkgid dep@(Dependency pkgname verrange)
                                     installedConstraint =
    Constraints.constrain pkgname constraint reason
  where
    constraint ver installed = ver `withinRange` verrange
                            && if installedConstraint then installed else True
    reason = ExcludedByPackageDependency pkgid dep installedConstraint

addTopLevelVersionConstraint :: PackageName -> VersionRange
                             -> Constraints
                             -> Satisfiable Constraints
                                  [PackageId] ExclusionReason
addTopLevelVersionConstraint pkgname verrange =
    Constraints.constrain pkgname constraint reason
  where
    constraint ver _installed = ver `withinRange` verrange
    reason = ExcludedByTopLevelConstraintVersion pkgname verrange

addTopLevelInstalledConstraint,
  addTopLevelSourceConstraint :: PackageName
                              -> Constraints
                              -> Satisfiable Constraints
                                   [PackageId] ExclusionReason
addTopLevelInstalledConstraint pkgname =
    Constraints.constrain pkgname constraint reason
  where
    constraint _ver installed = installed
    reason = ExcludedByTopLevelConstraintInstalled pkgname

addTopLevelSourceConstraint pkgname =
    Constraints.constrain pkgname constraint reason
  where
    constraint _ver installed = not installed
    reason = ExcludedByTopLevelConstraintSource pkgname


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
   | ExcludedByPackageDependency PackageId Dependency Bool

     -- | We excluded this version of the package because it did not satisfy
     -- a dependency given as an original top level input.
     --
   | ExcludedByTopLevelConstraintVersion   PackageName VersionRange
   | ExcludedByTopLevelConstraintInstalled PackageName
   | ExcludedByTopLevelConstraintSource    PackageName

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
showExclusionReason pkgid (ExcludedByPackageDependency pkgid' dep installedConstraint)
  = display pkgid ++ " was excluded because " ++ display pkgid' ++ " requires "
 ++ (if installedConstraint then "an installed instance of " else "")
 ++ displayDep dep
showExclusionReason pkgid (ExcludedByTopLevelConstraintVersion pkgname verRange) =
  display pkgid ++ " was excluded because of the top level constraint " ++
  displayDep (Dependency pkgname verRange)
showExclusionReason pkgid (ExcludedByTopLevelConstraintInstalled pkgname)
  = display pkgid ++ " was excluded because of the top level constraint '"
 ++ display pkgname ++ " installed' which means that only installed instances "
 ++ "of the package may be selected."
showExclusionReason pkgid (ExcludedByTopLevelConstraintSource pkgname)
  = display pkgid ++ " was excluded because of the top level constraint '"
 ++ display pkgname ++ " source' which means that only source versions "
 ++ "of the package may be selected."


-- ------------------------------------------------------------
-- * Logging progress and failures
-- ------------------------------------------------------------

data Log = Select [SelectedPackage] [PackageId]
         | AppliedVersionConstraint   PackageName VersionRange [PackageId]
         | AppliedInstalledConstraint PackageName InstalledConstraint [PackageId]
         | ExcludeUnconfigurable PackageId

data Failure
   = NoSuchPackage
       PackageName
   | ConfigureFailed
       SelectablePackage
       [(Dependency, [(PackageId, [ExclusionReason])])]
   | DependencyConflict
       SelectedPackage Dependency Bool
       [(PackageId, [ExclusionReason])]
   | TopLevelVersionConstraintConflict
       PackageName VersionRange
       [(PackageId, [ExclusionReason])]
   | TopLevelVersionConstraintUnsatisfiable
       PackageName VersionRange
   | TopLevelInstallConstraintConflict
       PackageName InstalledConstraint
       [(PackageId, [ExclusionReason])]
   | TopLevelInstallConstraintUnsatisfiable
       PackageName InstalledConstraint

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

    kind (InstalledOnly _)        = "(installed)"
    kind (SourceOnly _)           = "(source)"
    kind (InstalledAndSource _ _) = "(installed or source)"

    discardedMsg = case discarded of
      []  -> ""
      _   -> "discarding " ++ listOf id
        [ element
        | (pkgid:pkgids) <- groupBy (equating packageName) (sort discarded)
        , element <- display pkgid : map (display . packageVersion) pkgids ]
showLog (AppliedVersionConstraint pkgname ver pkgids) =
     "applying constraint " ++ display (Dependency pkgname ver)
  ++ if null pkgids
       then ""
       else " which excludes " ++ listOf display pkgids
showLog (AppliedInstalledConstraint pkgname inst pkgids) =
     "applying constraint " ++ display pkgname ++ " '"
  ++ (case inst of InstalledConstraint -> "installed"; _ -> "source") ++ "' "
  ++ if null pkgids
       then ""
       else "which excludes " ++ listOf display pkgids
showLog (ExcludeUnconfigurable pkgid) =
     "excluding " ++ display pkgid ++ " (it cannot be configured)"

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

showFailure (DependencyConflict pkg dep installedConstraint conflicts) =
     "dependencies conflict: "
  ++ displayPkg pkg ++ " requires "
  ++ (if installedConstraint then "an installed instance of " else "")
  ++ displayDep dep ++ " however:\n"
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

showFailure (TopLevelInstallConstraintConflict name InstalledConstraint conflicts) =
     "constraints conflict: "
  ++ "top level constraint '" ++ display name ++ " installed' however\n"
  ++ unlines [ showExclusionReason (packageId pkg') reason
             | (pkg', reasons) <- conflicts, reason <- reasons ]

showFailure (TopLevelInstallConstraintUnsatisfiable name InstalledConstraint) =
     "There is no installed version of " ++ display name

showFailure (TopLevelInstallConstraintConflict name SourceConstraint conflicts) =
     "constraints conflict: "
  ++ "top level constraint '" ++ display name ++ " source' however\n"
  ++ unlines [ showExclusionReason (packageId pkg') reason
             | (pkg', reasons) <- conflicts, reason <- reasons ]

showFailure (TopLevelInstallConstraintUnsatisfiable name SourceConstraint) =
     "There is no available source version of " ++ display name

displayVer :: VersionRange -> String
displayVer = display . simplifyVersionRange

displayDep :: Dependency -> String
displayDep = display . simplifyDependency


-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

impossible :: String -> a
impossible msg = internalError $ "assertion failure: " ++ msg

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

-- ------------------------------------------------------------
-- * Construct a dependency graph
-- ------------------------------------------------------------

-- | Builds a graph of the package dependencies.
--
-- Dependencies on other packages that are not in the index are discarded.
-- You can check if there are any such dependencies with 'brokenPackages'.
--
-- The top-down solver gets its own implementation, because both
-- `dependencyGraph` in `Distribution.Client.PlanIndex` (in cabal-install) and
-- `dependencyGraph` in `Distribution.Simple.PackageIndex` (in Cabal) both work
-- with `PackageIndex` from `Cabal` (that is, a package index indexed by
-- installed package IDs rather than package names).
--
-- Ideally we would switch the top-down solver over to use that too, so that
-- this duplication could be avoided, but that's a bit of work and the top-down
-- solver is legacy code anyway.
--
-- (NOTE: This is called at two types: InstalledPackage and FinalSelectedPackage.)
dependencyGraph :: PackageSourceDeps pkg
                => PackageIndex pkg
                -> (Graph.Graph,
                    Graph.Vertex -> pkg,
                    PackageId -> Maybe Graph.Vertex)
dependencyGraph index = (graph, vertexToPkg, pkgIdToVertex)
  where
    graph = Array.listArray bounds $
            map (catMaybes . map pkgIdToVertex . sourceDeps) pkgs
    vertexToPkg vertex = pkgTable Array.! vertex
    pkgIdToVertex = binarySearch 0 topBound

    pkgTable   = Array.listArray bounds pkgs
    pkgIdTable = Array.listArray bounds (map packageId pkgs)
    pkgs = sortBy (comparing packageId) (PackageIndex.allPackages index)
    topBound = length pkgs - 1
    bounds = (0, topBound)

    binarySearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (pkgIdTable Array.! mid) of
          LT -> binarySearch a (mid-1) key
          EQ -> Just mid
          GT -> binarySearch (mid+1) b key
      where mid = (a + b) `div` 2
