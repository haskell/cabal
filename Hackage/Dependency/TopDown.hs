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
import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..)
         , ConfiguredPackage(..) )
import Hackage.Dependency.Types
         ( DependencyResolver, Progress )
import qualified Hackage.Dependency.Types as Progress
         ( Progress(..), foldProgress )

import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Package
         ( PackageIdentifier(PackageIdentifier), Dependency(Dependency)
         , Package(..), packageVersion )
import Distribution.PackageDescription
         ( PackageDescription(buildDepends) )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription)
import Distribution.Version
         ( VersionRange(..) )
import Distribution.Compiler
         ( CompilerId )
import Distribution.System
         ( OS, Arch )
import Distribution.Simple.Utils
         ( equating, comparing )
import Distribution.Text
         ( display )

import Data.List
         ( maximumBy, deleteBy )
import Data.Maybe
         ( fromJust )
import Data.Monoid
         ( Monoid(mempty) )
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Graph as Graph

import Debug.Trace (trace)

-- ------------------------------------------------------------
-- * Search state types
-- ------------------------------------------------------------

type Constraints  = Constraints.Constraints
                      InstalledPackage AvailablePackage ExclusionReason
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

explore :: SearchSpace a SelectablePackage
        -> Progress Log Failure a

explore (Failure failure)      = Progress.Fail failure
explore (ChoiceNode result []) = Progress.Done result
explore (ChoiceNode _ (choices@(defaultChoice:_))) =
  case [ choice | [choice] <- choices ] of
    ((pkg, node'):_) -> Progress.Step (Select pkg [])    (explore node')
    []               -> seq pkgs' -- avoid retaining defaultChoice
                      $ Progress.Step (Select pkg pkgs') (explore node')
      where
        (pkg, node') = maximumBy (comparing (packageId . fst)) defaultChoice
        pkgs' = deleteBy (equating packageId) pkg (map fst defaultChoice)

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
      Right pkg' -> case constrainDeps pkg' (packageConstraints pkg') constraints of
        Left failure       -> Failure failure
        Right constraints' -> searchSpace configure constraints'' selected' next'
          where
            selected' = PackageIndex.insert pkg' selected
            next'     = Set.delete name $ foldr Set.insert next new
            new       = [ name'
                        | dep <- packageConstraints pkg'
                        , let (Dependency name' _) = untagDependency dep
                        , null (PackageIndex.lookupPackageName selected' name') ]
            Satisfiable constraints'' = addPackageSelectConstraint (packageId pkg) constraints'

packageConstraints :: SelectedPackage -> [TaggedDependency]
packageConstraints = either installedConstraints availableConstraints
                   . preferAvailable
  where
    preferAvailable (InstalledOnly           pkg) = Left pkg
    preferAvailable (AvailableOnly           pkg) = Right pkg
    preferAvailable (InstalledAndAvailable _ pkg) = Right pkg
    installedConstraints (InstalledPackage      _   deps) =
      [ TaggedDependency InstalledConstraint (thisPackageVersion dep)
      | dep <- deps ]
    availableConstraints (SemiConfiguredPackage _ _ deps) =
      [ TaggedDependency NoInstalledConstraint dep | dep <- deps ]

constrainDeps :: SelectedPackage -> [TaggedDependency] -> Constraints
              -> Either Failure Constraints
constrainDeps _   []         cs = Right cs
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
       -> Constraints
       -> Set PackageName
       -> Progress Log Failure (SelectedPackages, Constraints)
search configure constraints =
  explore . searchSpace configure constraints mempty

-- ------------------------------------------------------------
-- * The top level resolver
-- ------------------------------------------------------------

-- | The main exported resolver, with string logging and failure types to fit
-- the standard 'DependencyResolver' interface.
--
topDownResolver :: DependencyResolver a
topDownResolver = (((((mapMessages .).).).).) . topDownResolver'
  where
    mapMessages :: Progress Log Failure a -> Either [Dependency] a
    mapMessages = Progress.foldProgress (trace . showLog)
                                        (error . showFailure)
                                        Right

-- | The native resolver with detailed structured logging and failure types.
--
topDownResolver' :: OS -> Arch -> CompilerId
                 -> PackageIndex InstalledPackageInfo
                 -> PackageIndex AvailablePackage
                 -> [UnresolvedDependency]
                 -> Progress Log Failure [InstallPlan.PlanPackage a]
topDownResolver' os arch comp installed available deps =
    fmap (uncurry finaliseSelectedPackages)
  $ search (configurePackage os arch comp) constraints initialPkgNames

  where
    --TODO add actual constraints using addTopLevelDependencyConstraint
    constraints = Constraints.empty installed' available

    installed' = PackageIndex.fromList
                   [ InstalledPackage pkg (transitiveDepends pkg)
                   | pkg <- PackageIndex.allPackages installed ]   
    transitiveDepends :: InstalledPackageInfo -> [PackageIdentifier]
    transitiveDepends = map toPkgid . tail . Graph.reachable graph
                      . fromJust . toVertex . packageId
    (graph, toPkgid, toVertex) = PackageIndex.dependencyGraph installed

    initialDeps     = [ dep  | UnresolvedDependency dep _ <- deps ]
    initialPkgNames = Set.fromList [ name | Dependency name _ <- initialDeps ]

configurePackage :: OS -> Arch -> CompilerId -> ConfigurePackage
configurePackage os arch comp available spkg = case spkg of
  InstalledOnly         ipkg      -> Right (InstalledOnly ipkg)
  AvailableOnly              apkg -> fmap AvailableOnly (configure apkg)
  InstalledAndAvailable ipkg apkg -> fmap (InstalledAndAvailable ipkg)
                                          (configure apkg)
  where
  configure apkg@(AvailablePackage _ p _) =
    case finalizePackageDescription [] (Just available) os arch comp [] p of
      Left missing        -> Left missing
      Right (pkg, flags) -> Right $
        SemiConfiguredPackage apkg flags (buildDepends pkg)

finaliseSelectedPackages :: SelectedPackages
                         -> Constraints
                         -> [InstallPlan.PlanPackage a]
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
        --TODO: improve the plan by picking installed packages where possible

    finaliseInstalled (InstalledPackage pkg _) = InstallPlan.PreExisting pkg
    finaliseAvailable (SemiConfiguredPackage pkg flags deps) =
      InstallPlan.Configured (ConfiguredPackage pkg flags deps')
      where deps' = [ packageId pkg'
                    | dep <- deps
                    , let pkg' = case PackageIndex.lookupDependency selected dep of
                                   [pkg''] -> pkg''
                                   _ -> impossible ]

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

thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) = Dependency n (ThisVersion v)

addPackageExcludeConstraint :: PackageIdentifier -> Constraints
                     -> Satisfiable Constraints ExclusionReason
addPackageExcludeConstraint pkgid constraints =
  Constraints.constrain dep reason constraints
  where
    dep    = TaggedDependency NoInstalledConstraint
               (notThisPackageVersion pkgid)
    reason = ExcludedByConfigureFail
    notThisPackageVersion (PackageIdentifier n v) =
      Dependency n (notThisVersion v)
    notThisVersion v = UnionVersionRanges (EarlierVersion v) (LaterVersion v)

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
