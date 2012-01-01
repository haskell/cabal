-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency.TopDown.Constraints
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A set of satisfiable constraints on a set of packages.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency.TopDown.Constraints (
  Constraints,
  empty,
  packages,
  choices,
  isPaired,

  addTarget,
  constrain,
  Satisfiable(..),
  conflicting,
  ) where

import Distribution.Client.Dependency.TopDown.Types
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Client.PackageIndex (PackageIndex)
import Distribution.Package
         ( PackageName, PackageId, PackageIdentifier(..)
         , Package(packageId), packageName, packageVersion
         , Dependency, PackageFixedDeps(depends) )
import Distribution.Version
         ( Version )
import Distribution.Client.Utils
         ( mergeBy, MergeResult(..) )

import Data.Monoid
         ( Monoid(mempty) )
import Data.Either
         ( partitionEithers )
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Exception
         ( assert )


-- | A set of satisfiable constraints on a set of packages.
--
-- The 'Constraints' type keeps track of a set of targets (identified by
-- package name) that we know that we need. It also keeps track of a set of
-- constraints over all packages in the environment.
--
-- It maintains the guarantee that, for the target set, the constraints are
-- satisfiable, meaning that there is at least one instance available for each
-- package name that satisfies the constraints on that package name.
--
-- Note that it is possible to over-constrain a package in the environment that
-- is not in the target set -- the satisfiability guarantee is only maintained
-- for the target set. This is useful because it allows us to exclude packages
-- without needing to know if it would ever be needed or not (e.g. allows
-- excluding broken installed packages).
--
-- Adding a constraint for a target package can fail if it would mean that
-- there are no remaining choices.
--
-- Adding a constraint for package that is not a target never fails.
--
-- Adding a new target package can fail if that package already has conflicting
-- constraints.
--
data Constraints installed source reason
   = Constraints

       -- | Targets that we know we need. This is the set for which we
       -- guarantee the constraints are satisfiable.
       !(Set PackageName)

       -- | The available/remaining set. These are packages that have available
       -- choices remaining. This is guaranteed to cover the target packages,
       -- but can also cover other packages in the environment. New targets can
       -- only be added if there are available choices remaining for them.
       !(PackageIndex (InstalledOrSource installed source))

       -- | The excluded set. Choices that we have excluded by applying
       -- constraints. Excluded choices are tagged with the reason.
       !(PackageIndex (ExcludedPkg (InstalledOrSource installed source) reason))

       -- | Paired choices, this is an ugly hack.
       !(Map PackageName (Version, Version))

       -- | Purely for the invariant, we keep a copy of the original index
       !(PackageIndex (InstalledOrSource installed source))


-- | Reasons for excluding all, or some choices for a package version.
--
-- Each package version can have a source instance, an installed instance or
-- both. We distinguish reasons for constraints that excluded both instances,
-- from reasons for constraints that excluded just one instance.
--
data ExcludedPkg pkg reason
   = ExcludedPkg pkg
       [reason] -- ^ reasons for excluding both source and installed instances
       [reason] -- ^ reasons for excluding the installed instance
       [reason] -- ^ reasons for excluding the source instance

instance Package pkg => Package (ExcludedPkg pkg reason) where
  packageId (ExcludedPkg p _ _ _) = packageId p


-- | There is a conservation of packages property. Packages are never gained or
-- lost, they just transfer from the remaining set to the excluded set.
--
invariant :: (Package installed, Package source)
          => Constraints installed source a -> Bool
invariant (Constraints targets available excluded _ original) =

    -- Relationship between available, excluded and original
    all check merged

    -- targets is a subset of available
 && all (PackageIndex.elemByPackageName available) (Set.elems targets)

  where
    merged = mergeBy (\a b -> packageId a `compare` mergedPackageId b)
                     (PackageIndex.allPackages original)
                     (mergeBy (\a b -> packageId a `compare` packageId b)
                              (PackageIndex.allPackages available)
                              (PackageIndex.allPackages excluded))
      where
        mergedPackageId (OnlyInLeft  p  ) = packageId p
        mergedPackageId (OnlyInRight   p) = packageId p
        mergedPackageId (InBoth      p _) = packageId p

    -- If the package was originally installed only, then
    check (InBoth (InstalledOnly _) cur) = case cur of
      -- now it's either still remaining as installed only
      OnlyInLeft               (InstalledOnly _)              -> True
      -- or it has been excluded
      OnlyInRight (ExcludedPkg (InstalledOnly _) [] (_:_) []) -> True
      _                                                       -> False

    -- If the package was originally available only, then
    check (InBoth (SourceOnly _) cur) = case cur of
      -- now it's either still remaining as source only
      OnlyInLeft               (SourceOnly _)              -> True
      -- or it has been excluded
      OnlyInRight (ExcludedPkg (SourceOnly _) [] [] (_:_)) -> True
      _                                                    -> False

    -- If the package was originally installed and source, then
    check (InBoth (InstalledAndSource _ _) cur) = case cur of
      -- We can have both remaining:
      OnlyInLeft               (InstalledAndSource _ _)        -> True

      -- both excluded, in particular it can have had the just source or
      -- installed excluded and later had both excluded so we do not mind if
      -- the source or installed excluded is empty or non-empty.
      OnlyInRight (ExcludedPkg (InstalledAndSource _ _) _ _ _) -> True

      -- the installed remaining and the source excluded:
      InBoth                   (InstalledOnly _)
                  (ExcludedPkg (SourceOnly _) [] [] (_:_))     -> True

      -- the source remaining and the installed excluded:
      InBoth                   (SourceOnly _)
                  (ExcludedPkg (InstalledOnly _) [] (_:_) [])  -> True
      _                                                        -> False

    check _ = False


-- | An update to the constraints can move packages between the two piles
-- but not gain or loose packages.
transitionsTo :: (Package installed, Package source)
              => Constraints installed source a
              -> Constraints installed source a -> Bool
transitionsTo constraints @(Constraints _ available  excluded  _ _)
              constraints'@(Constraints _ available' excluded' _ _) =

     invariant constraints && invariant constraints'
  && null availableGained  && null excludedLost
  &&    map (mapInstalledOrSource packageId packageId) availableLost
     == map (mapInstalledOrSource packageId packageId) excludedGained

  where
    (availableLost, availableGained)
      = partitionEithers (foldr lostAndGained [] availableChange)

    (excludedLost, excludedGained)
      = partitionEithers (foldr lostAndGained [] excludedChange)

    availableChange =
      mergeBy (\a b -> packageId a `compare` packageId b)
        (PackageIndex.allPackages available)
        (PackageIndex.allPackages available')

    excludedChange =
      mergeBy (\a b -> packageId a `compare` packageId b)
        [ pkg | ExcludedPkg pkg _ _ _ <- PackageIndex.allPackages excluded  ]
        [ pkg | ExcludedPkg pkg _ _ _ <- PackageIndex.allPackages excluded' ]

    lostAndGained mr rest = case mr of
      OnlyInLeft pkg                    -> Left pkg : rest
      InBoth (InstalledAndSource pkg _)
             (SourceOnly _)             -> Left (InstalledOnly pkg) : rest
      InBoth (InstalledAndSource _ pkg)
             (InstalledOnly _)          -> Left (SourceOnly pkg) : rest
      InBoth (SourceOnly _)
             (InstalledAndSource pkg _) -> Right (InstalledOnly pkg) : rest
      InBoth (InstalledOnly _)
             (InstalledAndSource _ pkg) -> Right (SourceOnly pkg) : rest
      OnlyInRight pkg                   -> Right pkg : rest
      _                                 -> rest

    mapInstalledOrSource f g pkg = case pkg of
      InstalledOnly      a   -> InstalledOnly (f a)
      SourceOnly           b -> SourceOnly    (g b)
      InstalledAndSource a b -> InstalledAndSource (f a) (g b)


-- | We construct 'Constraints' with an initial 'PackageIndex' of all the
-- packages available.
--
empty :: (PackageFixedDeps installed, Package source)
      => PackageIndex installed
      -> PackageIndex source
      -> Constraints installed source reason
empty installed source =
    Constraints targets pkgs excluded pairs pkgs
  where
    targets  = mempty
    excluded = mempty
    pkgs = PackageIndex.fromList
         . map toInstalledOrSource
         $ mergeBy (\a b -> packageId a `compare` packageId b)
                   (PackageIndex.allPackages installed)
                   (PackageIndex.allPackages source)
    toInstalledOrSource (OnlyInLeft  i  ) = InstalledOnly      i
    toInstalledOrSource (OnlyInRight   a) = SourceOnly           a
    toInstalledOrSource (InBoth      i a) = InstalledAndSource i a

    -- pick up cases like base-3 and 4 where one version depends on the other:
    pairs = Map.fromList
      [ (name, (packageVersion pkgid1, packageVersion pkgid2))
      | [pkg1, pkg2] <- PackageIndex.allPackagesByName installed
      , let name   = packageName pkg1
            pkgid1 = packageId pkg1
            pkgid2 = packageId pkg2
      ,    any ((pkgid1==) . packageId) (depends pkg2)
        || any ((pkgid2==) . packageId) (depends pkg1) ]


-- | The package targets.
--
packages :: (Package installed, Package source)
         => Constraints installed source reason
         -> Set PackageName
packages (Constraints ts _ _ _ _) = ts


-- | The package choices that are still available.
--
choices :: (Package installed, Package source)
        => Constraints installed source reason
        -> PackageIndex (InstalledOrSource installed source)
choices (Constraints _ available _ _ _) = available

isPaired :: (Package installed, Package source)
         => Constraints installed source reason
         -> PackageId -> Maybe PackageId
isPaired (Constraints _ _ _ pairs _) (PackageIdentifier name version) =
  case Map.lookup name pairs of
    Just (v1, v2)
      | version == v1 -> Just (PackageIdentifier name v2)
      | version == v2 -> Just (PackageIdentifier name v1)
    _                 -> Nothing


data Satisfiable constraints discarded reason
       = Satisfiable constraints discarded
       | Unsatisfiable
       | ConflictsWith [(PackageId, [reason])]


addTarget :: (Package installed, Package source)
          => PackageName
          -> Constraints installed source reason
          -> Satisfiable (Constraints installed source reason)
                         () reason
addTarget pkgname
          constraints@(Constraints targets available excluded paired original)

    -- If it's already a target then there's no change
  | pkgname `Set.member` targets
  = Satisfiable constraints ()

    -- If there is some possible choice available for this target then we're ok
  | PackageIndex.elemByPackageName available pkgname
  = let targets'     = Set.insert pkgname targets
        constraints' = Constraints targets' available excluded paired original
     in assert (constraints `transitionsTo` constraints') $
        Satisfiable constraints' ()

    -- If it's not available and it is excluded then we return the conflicts
  | PackageIndex.elemByPackageName excluded pkgname
  = ConflictsWith conflicts

    -- Otherwise, it's not available and it has not been excluded so the
    -- package is simply completely unknown.
  | otherwise
  = Unsatisfiable

  where
    conflicts =
      [ (packageId pkg, reasons)
      | let excludedChoices = PackageIndex.lookupPackageName excluded pkgname
      , ExcludedPkg pkg isReasons iReasons sReasons <- excludedChoices
      , let reasons = isReasons ++ iReasons ++ sReasons ]


constrain :: (Package installed, Package source)
          => PackageName                -- ^ which package to constrain
          -> (Version -> Bool -> Bool)  -- ^ the constraint test
          -> reason                     -- ^ the reason for the constraint
          -> Constraints installed source reason
          -> Satisfiable (Constraints installed source reason)
                         [PackageId] reason
constrain pkgname constraint reason
          constraints@(Constraints targets available excluded paired original)

  | pkgname `Set.member` targets  &&  not anyRemaining
  = if null conflicts then Unsatisfiable
                      else ConflictsWith conflicts

  | otherwise
  = let constraints' = Constraints targets available' excluded' paired original
     in assert (constraints `transitionsTo` constraints') $
        Satisfiable constraints' (map packageId newExcluded)

  where
    -- This tells us if any packages would remain at all for this package name if
    -- we applied this constraint. This amounts to checking if any package
    -- satisfies the given constraint, including version range and installation
    -- status.
    --
    (available', excluded', newExcluded, anyRemaining, conflicts) =
      updatePkgsStatus
        available excluded
        [] False []
        (mergeBy (\pkg pkg' -> packageVersion pkg `compare` packageVersion pkg')
                 (PackageIndex.lookupPackageName available pkgname)
                 (PackageIndex.lookupPackageName excluded  pkgname))

    testConstraint pkg =
      let ver = packageVersion pkg in
      case Map.lookup (packageName pkg) paired of

        Just (v1, v2)
          | ver == v1 || ver == v2
          -> case pkg of
               InstalledOnly ipkg -> InstalledOnly (ipkg, iOk)
               SourceOnly    spkg -> SourceOnly    (spkg, sOk)
               InstalledAndSource ipkg spkg ->
                 InstalledAndSource (ipkg, iOk) (spkg, sOk)
          where
            iOk = constraint v1 True  || constraint v2 True
            sOk = constraint v1 False || constraint v2 False

        _ -> case pkg of
               InstalledOnly ipkg -> InstalledOnly (ipkg, iOk)
               SourceOnly    spkg -> SourceOnly    (spkg, sOk)
               InstalledAndSource ipkg spkg ->
                 InstalledAndSource (ipkg, iOk) (spkg, sOk)
          where
            iOk = constraint ver True
            sOk = constraint ver False

    -- For the info about available and excluded versions of the package in
    -- question, update the info given the current constraint
    --
    -- We update the available package map and the excluded package map
    -- we also collect:
    --   * the change in available packages (for logging)
    --   * whether there are any remaining choices
    --   * any constraints that conflict with the current constraint

    updatePkgsStatus _ _ nePkgs ok cs _
      | seq nePkgs $ seq ok $ seq cs False = undefined

    updatePkgsStatus aPkgs ePkgs nePkgs ok cs []
      = (aPkgs, ePkgs, reverse nePkgs, ok, reverse cs)

    updatePkgsStatus aPkgs ePkgs nePkgs ok cs (pkg:pkgs) =
        let (aPkgs', ePkgs', mnePkg, ok', mc) = updatePkgStatus aPkgs ePkgs pkg
            nePkgs' = maybeCons mnePkg nePkgs
            cs'     = maybeCons mc cs
         in updatePkgsStatus aPkgs' ePkgs' nePkgs' (ok' || ok) cs' pkgs

    maybeCons Nothing  xs = xs
    maybeCons (Just x) xs = x:xs


    -- For the info about an available or excluded version of the package in
    -- question, update the info given the current constraint.
    --
    updatePkgStatus aPkgs ePkgs pkg =
      case viewPackageStatus pkg of
        AllAvailable (InstalledOnly (aiPkg, False)) ->
          removeAvailable False
            (InstalledOnly aiPkg)
            (PackageIndex.deletePackageId pkgid)
            (ExcludedPkg (InstalledOnly aiPkg) [] [reason] [])
            Nothing

        AllAvailable (SourceOnly (asPkg, False)) ->
          removeAvailable False
            (SourceOnly asPkg)
            (PackageIndex.deletePackageId pkgid)
            (ExcludedPkg (SourceOnly asPkg) [] [] [reason])
            Nothing

        AllAvailable (InstalledAndSource (aiPkg, False) (asPkg, False)) ->
          removeAvailable False
            (InstalledAndSource aiPkg asPkg)
            (PackageIndex.deletePackageId pkgid)
            (ExcludedPkg (InstalledAndSource aiPkg asPkg) [reason] [] [])
            Nothing

        AllAvailable (InstalledAndSource (aiPkg, True) (asPkg, False)) ->
          removeAvailable True
            (SourceOnly asPkg)
            (PackageIndex.insert (InstalledOnly aiPkg))
            (ExcludedPkg (SourceOnly asPkg) [] [] [reason])
            Nothing

        AllAvailable (InstalledAndSource (aiPkg, False) (asPkg, True)) ->
          removeAvailable True
            (InstalledOnly aiPkg)
            (PackageIndex.insert (SourceOnly asPkg))
            (ExcludedPkg (InstalledOnly aiPkg) [] [reason] [])
            Nothing

        AllAvailable _ -> noChange True Nothing

        AvailableExcluded (aiPkg, False) (ExcludedPkg (esPkg, False) _ _ srs) ->
          removeAvailable False
            (InstalledOnly aiPkg)
            (PackageIndex.deletePackageId pkgid)
            (ExcludedPkg (InstalledAndSource aiPkg esPkg) [reason] [] srs)
            Nothing

        AvailableExcluded (_aiPkg, True) (ExcludedPkg (esPkg, False) _ _ srs) ->
          addExtraExclusion True
            (ExcludedPkg (SourceOnly esPkg) [] [] (reason:srs))
            Nothing

        AvailableExcluded (aiPkg, False) (ExcludedPkg (esPkg, True) _ _ srs) ->
          removeAvailable  True
            (InstalledOnly aiPkg)
            (PackageIndex.deletePackageId pkgid)
            (ExcludedPkg (InstalledAndSource aiPkg esPkg) [] [reason] srs)
            (Just (pkgid, srs))

        AvailableExcluded (_aiPkg, True) (ExcludedPkg (_esPkg, True) _ _ srs) ->
          noChange True
            (Just (pkgid, srs))

        ExcludedAvailable (ExcludedPkg (eiPkg, False) _ irs _) (asPkg, False) ->
          removeAvailable  False
            (SourceOnly asPkg)
            (PackageIndex.deletePackageId pkgid)
            (ExcludedPkg (InstalledAndSource eiPkg asPkg) [reason] irs [])
            Nothing

        ExcludedAvailable (ExcludedPkg (eiPkg, True) _ irs _) (asPkg, False) ->
          removeAvailable False
            (SourceOnly asPkg)
            (PackageIndex.deletePackageId pkgid)
            (ExcludedPkg (InstalledAndSource eiPkg asPkg) [] irs [reason])
            (Just (pkgid, irs))

        ExcludedAvailable (ExcludedPkg (eiPkg, False) _ irs _) (_asPkg, True) ->
          addExtraExclusion True
            (ExcludedPkg (InstalledOnly eiPkg) [] (reason:irs) [])
            Nothing

        ExcludedAvailable (ExcludedPkg (_eiPkg, True) _ irs _) (_asPkg, True) ->
          noChange True
            (Just (pkgid, irs))

        AllExcluded (ExcludedPkg (InstalledOnly (eiPkg, False)) _ irs _) ->
          addExtraExclusion False
            (ExcludedPkg (InstalledOnly eiPkg) [] (reason:irs) [])
            Nothing

        AllExcluded (ExcludedPkg (InstalledOnly (_eiPkg, True)) _ irs _) ->
          noChange False
            (Just (pkgid, irs))

        AllExcluded (ExcludedPkg (SourceOnly (esPkg, False)) _ _ srs) ->
          addExtraExclusion False
            (ExcludedPkg (SourceOnly esPkg) [] [] (reason:srs))
            Nothing

        AllExcluded (ExcludedPkg (SourceOnly (_esPkg, True)) _ _ srs) ->
          noChange False
            (Just (pkgid, srs))

        AllExcluded (ExcludedPkg (InstalledAndSource (eiPkg, False) (esPkg, False)) isrs irs srs) ->
          addExtraExclusion False
            (ExcludedPkg (InstalledAndSource eiPkg esPkg) (reason:isrs) irs srs)
            Nothing

        AllExcluded (ExcludedPkg (InstalledAndSource (eiPkg, True) (esPkg, False)) isrs irs srs) ->
          addExtraExclusion False
            (ExcludedPkg (InstalledAndSource eiPkg esPkg) isrs irs (reason:srs))
            (Just (pkgid, irs))

        AllExcluded (ExcludedPkg (InstalledAndSource (eiPkg, False) (esPkg, True)) isrs irs srs) ->
          addExtraExclusion False
            (ExcludedPkg (InstalledAndSource eiPkg esPkg) isrs (reason:irs) srs)
            (Just (pkgid, srs))

        AllExcluded (ExcludedPkg (InstalledAndSource (_eiPkg, True) (_esPkg, True)) isrs irs srs) ->
          noChange False
            (Just (pkgid, isrs ++ irs ++ srs))

      where
        removeAvailable ok nePkg adjustAvailable ePkg c =
          let aPkgs' = adjustAvailable aPkgs
              ePkgs' = PackageIndex.insert ePkg ePkgs
           in aPkgs' `seq` ePkgs' `seq`
              (aPkgs', ePkgs', Just nePkg, ok, c)

        addExtraExclusion ok ePkg c =
          let ePkgs' = PackageIndex.insert ePkg ePkgs
           in ePkgs' `seq`
              (aPkgs, ePkgs', Nothing, ok, c)

        noChange ok c =
          (aPkgs, ePkgs, Nothing, ok, c)

        pkgid = case pkg of OnlyInLeft  p   -> packageId p
                            OnlyInRight p   -> packageId p
                            InBoth      p _ -> packageId p


    viewPackageStatus
      :: (Package installed, Package source)
      => MergeResult (InstalledOrSource installed source)
                     (ExcludedPkg (InstalledOrSource installed source) reason)
      -> PackageStatus (installed, Bool) (source, Bool) reason
    viewPackageStatus merged =
        case merged of
          OnlyInLeft aPkg ->
            AllAvailable (testConstraint aPkg)

          OnlyInRight (ExcludedPkg ePkg isrs irs srs) ->
            AllExcluded (ExcludedPkg (testConstraint ePkg) isrs irs srs)

          InBoth (InstalledOnly aiPkg)
                 (ExcludedPkg (SourceOnly esPkg) [] [] srs) ->
            case testConstraint (InstalledAndSource aiPkg esPkg) of
              InstalledAndSource (aiPkg', iOk) (esPkg', sOk) ->
                AvailableExcluded (aiPkg', iOk) (ExcludedPkg (esPkg', sOk) [] [] srs)
              _ -> impossible

          InBoth (SourceOnly asPkg)
                 (ExcludedPkg (InstalledOnly eiPkg) [] irs []) ->
            case testConstraint (InstalledAndSource eiPkg asPkg) of
              InstalledAndSource (eiPkg', iOk) (asPkg', sOk) ->
                ExcludedAvailable (ExcludedPkg (eiPkg', iOk) [] irs []) (asPkg', sOk)
              _ -> impossible
          _ -> impossible
      where
        impossible = error "impossible: viewPackageStatus invariant violation"

-- A intermediate structure that enumerates all the possible cases given the
-- invariant. This helps us to get simpler and complete pattern matching in
-- updatePkg above
--
data PackageStatus installed source reason
   = AllAvailable (InstalledOrSource installed source)
   | AllExcluded  (ExcludedPkg (InstalledOrSource installed source) reason)
   | AvailableExcluded installed (ExcludedPkg source reason)
   | ExcludedAvailable (ExcludedPkg installed reason) source


conflicting :: (Package installed, Package source)
            => Constraints installed source reason
            -> Dependency
            -> [(PackageId, [reason])]
conflicting (Constraints _ _ excluded _ _) dep =
  [ (packageId pkg, reasonsAll ++ reasonsAvail ++ reasonsInstalled) --TODO
  | ExcludedPkg pkg reasonsAll reasonsAvail reasonsInstalled <-
      PackageIndex.lookupDependency excluded dep ]
