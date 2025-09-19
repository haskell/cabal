{-# LANGUAGE BangPatterns #-}

module Distribution.Solver.Modular.Message (
    Message(..),
    summarizeMessages,
    renderSummarizedMessage,
  ) where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (catMaybes, mapMaybe, isJust)
import Prelude hiding (pi)

import Distribution.Pretty ( prettyShow ) -- from Cabal

import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.Dependency
    ( Var(P),
      ConflictSet,
      showConflictSet,
      QGoalReason,
      GoalReason(DependencyGoal, UserGoal),
      Goal(Goal),
      DependencyReason(DependencyReason),
      ExposedComponent(..),
      PkgComponent(PkgComponent),
      CI(Constrained, Fixed),
      showDependencyReason )
import Distribution.Solver.Modular.Flag
    ( QSN, QFN, showQFNBool, showQSNBool, showQFN, showQSN )
import Distribution.Solver.Modular.MessageUtils
    ( showUnsupportedExtension, showUnsupportedLanguage )
import Distribution.Solver.Modular.Package
    ( PI(PI), showI, showPI )
import Distribution.Solver.Modular.Tree
    ( FailReason(..), POption(..), ConflictingDep(..) )
import Distribution.Solver.Modular.Version
    ( VR, Ver, showVer, showVR, (.||.) )

import Distribution.Solver.Types.ConstraintSource
    ( ConstraintSource (..), showConstraintSource )
import Distribution.Solver.Types.PackagePath
    ( QPN, Qualified(Q), showQPN )
import Distribution.Solver.Types.Progress
    ( Progress(..) )
import Distribution.Solver.Types.ProjectConfigPath
    ( docProjectConfigPathFailReason)
import Distribution.Solver.Types.SummarizedMessage
    ( Entry(..), EntryAtLevel(..), SummarizedMessage(..) )
import Distribution.Types.LibraryName
    ( LibraryName(LSubLibName, LMainLibName) )
import Distribution.Types.UnqualComponentName
    ( unUnqualComponentName )
import Distribution.Verbosity (Verbosity, verbose)

import Text.PrettyPrint ( nest, render )

-- A data type to hold log information from the modular solver.
data Message =
    Enter           -- ^ increase indentation level
  | Leave           -- ^ decrease indentation level
  | TryP QPN POption
  | TryF QFN Bool
  | TryS QSN Bool
  | Next (Goal QPN)
  | Skip (Set CS.Conflict)
  | Success
  | Failure ConflictSet FailReason

renderSummarizedMessage :: Verbosity -> SummarizedMessage -> String
renderSummarizedMessage verb (SummarizedMsg i) = displayMessageAtLevel verb i
renderSummarizedMessage _ (StringMsg s) = s

displayMessageAtLevel :: Verbosity -> EntryAtLevel -> String
displayMessageAtLevel verb (AtLevel l msg) =
  let s = show l
  in  "[" ++ replicate (3 - length s) '_' ++ s ++ "] " ++ displayMessage verb msg

displayMessage :: Verbosity -> Entry -> String
displayMessage _ (EntryPackageGoal qpn gr) = "next goal: " ++ showQPN qpn ++ showGR gr
displayMessage _ (EntryRejectF qfn b c fr) = "rejecting: " ++ showQFNBool qfn b ++ showFR c fr
displayMessage _ (EntryRejectS qsn b c fr) = "rejecting: " ++ showQSNBool qsn b ++ showFR c fr
displayMessage _ (EntrySkipping cs) = "skipping: " ++ showConflicts cs
displayMessage _ (EntryTryingF qfn b) = "trying: " ++ showQFNBool qfn b
displayMessage _ (EntryTryingP qpn i) = "trying: " ++ showOption qpn i
displayMessage _ (EntryTryingNewP qpn i gr) = "trying: " ++ showOption qpn i ++ showGR gr
displayMessage _ (EntryTryingS qsn b) = "trying: " ++ showQSNBool qsn b
displayMessage _ (EntryUnknownPackage qpn gr) = "unknown package: " ++ showQPN qpn ++ showGR gr
displayMessage _ EntrySuccess = "done"
displayMessage _ (EntryFailure c fr) = "fail" ++ showFR c fr
displayMessage verb (EntrySkipMany qsn b cs) = "skipping: " ++ showOptions verb qsn b ++ " " ++ showConflicts cs
-- Instead of displaying `aeson-1.0.2.1, aeson-1.0.2.0, aeson-1.0.1.0, ...`,
-- the following line aims to display `aeson: 1.0.2.1, 1.0.2.0, 1.0.1.0, ...`.
--
displayMessage verb (EntryRejectMany qpn is c fr) = "rejecting: " ++ showOptions verb qpn is ++ showFR c fr

-- | Transforms the structured message type to actual messages (SummarizedMessage s).
--
-- The log contains level numbers, which are useful for any trace that involves
-- backtracking, because only the level numbers will allow to keep track of
-- backjumps.
summarizeMessages :: Progress Message a b -> Progress SummarizedMessage a b
summarizeMessages = go 0
  where
    -- 'go' increments the level for a recursive call when it encounters
    -- 'TryP', 'TryF', or 'TryS' and decrements the level when it encounters 'Leave'.
    go :: Int -> Progress Message a b -> Progress SummarizedMessage a b
    go !_ (Done x)                           = Done x
    go !_ (Fail x)                           = Fail x

    -- complex patterns
    go !l (Step (TryP qpn i) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        goPReject l qpn [i] c fr ms

    go !l (Step (TryP qpn i) (Step Enter (Step (Skip conflicts) (Step Leave ms)))) =
        goPSkip l qpn [i] conflicts ms

    go !l (Step (TryF qfn b) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        Step (SummarizedMsg $ AtLevel l $ (EntryRejectF qfn b c fr)) (go l ms)

    go !l (Step (TryS qsn b) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        Step (SummarizedMsg $ AtLevel l $ (EntryRejectS qsn b c fr)) (go l ms)

    -- "Trying ..." message when a new goal is started
    go !l (Step (Next (Goal (P _  ) gr)) (Step (TryP qpn' i) ms@(Step Enter (Step (Next _) _)))) =
        Step (SummarizedMsg $ AtLevel l $ (EntryTryingNewP qpn' i gr)) (go l ms)

    go !l (Step (Next (Goal (P qpn) gr)) (Step (Failure _c UnknownPackage) ms)) =
        Step (SummarizedMsg $ AtLevel l $ (EntryUnknownPackage qpn gr)) (go l ms)

    -- standard display
    go !l (Step Enter                    ms) = go (l+1) ms
    go !l (Step Leave                    ms) = go (l-1) ms

    go !l (Step (TryP qpn i)             ms) = Step (SummarizedMsg $ AtLevel l $ (EntryTryingP qpn i)) (go l ms)
    go !l (Step (TryF qfn b)             ms) = Step (SummarizedMsg $ AtLevel l $ (EntryTryingF qfn b)) (go l ms)
    go !l (Step (TryS qsn b)             ms) = Step (SummarizedMsg $ AtLevel l $ (EntryTryingS qsn b)) (go l ms)
    go !l (Step (Next (Goal (P qpn) gr)) ms) = Step (SummarizedMsg $ AtLevel l $ (EntryPackageGoal qpn gr)) (go l ms)
    go !l (Step (Next _)                 ms) = go l ms -- ignore flag goals in the log

    -- 'Skip' should always be handled by 'goPSkip' in the case above.
    go !l (Step (Skip conflicts)         ms) = Step (SummarizedMsg $ AtLevel l $ (EntrySkipping conflicts)) (go l ms)
    go !l (Step (Success)                ms) = Step (SummarizedMsg $ AtLevel l $ EntrySuccess) (go l ms)
    go !l (Step (Failure c fr)           ms) = Step (SummarizedMsg $ AtLevel l $ (EntryFailure c fr)) (go l ms)

    -- special handler for many subsequent package rejections
    goPReject :: Int
              -> QPN
              -> [POption]
              -> ConflictSet
              -> FailReason
              -> Progress Message a b
              -> Progress SummarizedMessage a b
    goPReject l qpn is c fr (Step (TryP qpn' i) (Step Enter (Step (Failure _ fr') (Step Leave ms))))
      | qpn == qpn' && fr == fr' =
        -- By prepending (i : is) we reverse the order of the instances.
        goPReject l qpn (i : is) c fr ms
    goPReject l qpn is c fr ms =
        Step (SummarizedMsg $ AtLevel l $ (EntryRejectMany qpn (reverse is) c fr)) (go l ms)

    -- Handle many subsequent skipped package instances.
    goPSkip :: Int
            -> QPN
            -> [POption]
            -> Set CS.Conflict
            -> Progress Message a b
            -> Progress SummarizedMessage a b
    goPSkip l qpn is conflicts (Step (TryP qpn' i) (Step Enter (Step (Skip conflicts') (Step Leave ms))))
      | qpn == qpn' && conflicts == conflicts' =
        -- By prepending (i : is) we reverse the order of the instances.
        goPSkip l qpn (i : is) conflicts ms
    goPSkip l qpn is conflicts ms =
       Step (SummarizedMsg $ AtLevel l $ (EntrySkipMany qpn (reverse is) conflicts)) (go l ms)

-- | Display the set of 'Conflicts' for a skipped package version.
showConflicts :: Set CS.Conflict -> String
showConflicts conflicts =
    "(has the same characteristics that caused the previous version to fail: "
     ++ conflictMsg ++ ")"
  where
    conflictMsg :: String
    conflictMsg =
      if S.member CS.OtherConflict conflicts
      then
        -- This case shouldn't happen, because an unknown conflict should not
        -- cause a version to be skipped.
        "unknown conflict"
      else let mergedConflicts =
                   [ showConflict qpn conflict
                   | (qpn, conflict) <- M.toList (mergeConflicts conflicts) ]
           in if L.null mergedConflicts
              then
                  -- This case shouldn't happen unless backjumping is turned off.
                  "none"
              else L.intercalate "; " mergedConflicts

    -- Merge conflicts to simplify the log message.
    mergeConflicts :: Set CS.Conflict -> Map QPN MergedPackageConflict
    mergeConflicts = M.fromListWith mergeConflict . mapMaybe toMergedConflict . S.toList
      where
        mergeConflict :: MergedPackageConflict
                      -> MergedPackageConflict
                      -> MergedPackageConflict
        mergeConflict mergedConflict1 mergedConflict2 = MergedPackageConflict {
              isGoalConflict =
                  isGoalConflict mergedConflict1 || isGoalConflict mergedConflict2
            , versionConstraintConflict =
                  L.nub $ versionConstraintConflict mergedConflict1
                       ++ versionConstraintConflict mergedConflict2
            , versionConflict =
                  mergeVersionConflicts (versionConflict mergedConflict1)
                                        (versionConflict mergedConflict2)
            }
          where
            mergeVersionConflicts (Just vr1) (Just vr2) = Just (vr1 .||. vr2)
            mergeVersionConflicts (Just vr1) Nothing    = Just vr1
            mergeVersionConflicts Nothing    (Just vr2) = Just vr2
            mergeVersionConflicts Nothing    Nothing    = Nothing

        toMergedConflict :: CS.Conflict -> Maybe (QPN, MergedPackageConflict)
        toMergedConflict (CS.GoalConflict qpn) =
            Just (qpn, MergedPackageConflict True [] Nothing)
        toMergedConflict (CS.VersionConstraintConflict qpn v) =
            Just (qpn, MergedPackageConflict False [v] Nothing)
        toMergedConflict (CS.VersionConflict qpn (CS.OrderedVersionRange vr)) =
            Just (qpn, MergedPackageConflict False [] (Just vr))
        toMergedConflict CS.OtherConflict = Nothing

    showConflict :: QPN -> MergedPackageConflict -> String
    showConflict qpn mergedConflict = L.intercalate "; " conflictStrings
      where
        conflictStrings = catMaybes [
            case () of
              () | isGoalConflict mergedConflict -> Just $
                     "depends on '" ++ showQPN qpn ++ "'" ++
                         (if null (versionConstraintConflict mergedConflict)
                          then ""
                          else " but excludes "
                                ++ showVersions (versionConstraintConflict mergedConflict))
                 | not $ L.null (versionConstraintConflict mergedConflict) -> Just $
                     "excludes '" ++ showQPN qpn
                      ++ "' " ++ showVersions (versionConstraintConflict mergedConflict)
                 | otherwise -> Nothing
          , (\vr -> "excluded by constraint '" ++ showVR vr ++ "' from '" ++ showQPN qpn ++ "'")
             <$> versionConflict mergedConflict
          ]

        showVersions []  = "no versions"
        showVersions [v] = "version " ++ showVer v
        showVersions vs  = "versions " ++ L.intercalate ", " (map showVer vs)

-- | All conflicts related to one package, used for simplifying the display of
-- a 'Set CS.Conflict'.
data MergedPackageConflict = MergedPackageConflict {
    isGoalConflict :: Bool
  , versionConstraintConflict :: [Ver]
  , versionConflict :: Maybe VR
  }

showOption :: QPN -> POption -> String
showOption qpn@(Q _pp pn) (POption i linkedTo) =
  case linkedTo of
    Nothing  -> showPI (PI qpn i) -- Consistent with prior to POption
    Just pp' -> showQPN qpn ++ "~>" ++ showPI (PI (Q pp' pn) i)

-- | Shows a mixed list of instances and versions in a human-friendly way,
-- abbreviated.
-- >>> showOptions verbose foobarQPN [v0, v1]
-- "foo-bar; 0, 1"
-- >>> showOptions verbose foobarQPN [v0]
-- "foo-bar-0"
-- >>> showOptions verbose foobarQPN [i0, i1]
-- "foo-bar; 0/installed-inplace, 1/installed-inplace"
-- >>> showOptions verbose foobarQPN [i0, v1]
-- "foo-bar; 0/installed-inplace, 1"
-- >>> showOptions verbose foobarQPN [v0, i1]
-- "foo-bar; 0, 1/installed-inplace"
-- >>> showOptions verbose foobarQPN []
-- "unexpected empty list of versions"
-- >>> showOptions verbose foobarQPN [k1, k2]
-- "foo-bar; foo-bar~>bazqux.foo-bar-1, foo-bar~>bazqux.foo-bar-2"
-- >>> showOptions verbose foobarQPN [v0, i1, k2]
-- "foo-bar; 0, 1/installed-inplace, foo-bar~>bazqux.foo-bar-2"
showOptions :: Verbosity -> QPN -> [POption] -> String
showOptions _ _ [] = "unexpected empty list of versions"
showOptions _ q [x] = showOption q x
showOptions verb q xs = showQPN q ++ "; " ++ (L.intercalate ", "
  [if isJust linkedTo
    then showOption q x
    else showI i -- Don't show the package, just the version
  | x@(POption i linkedTo) <- if verb >= verbose then xs else take 1 xs
  ] ++ if verb < verbose && length xs > 1 then " and " ++ show (length xs - 1) ++" other versions" else "")

showGR :: QGoalReason -> String
showGR UserGoal            = " (user goal)"
showGR (DependencyGoal dr) = " (dependency of " ++ showDependencyReason dr ++ ")"

showFR :: ConflictSet -> FailReason -> String
showFR _ (UnsupportedExtension ext)       = " (conflict: requires " ++ showUnsupportedExtension ext ++ ")"
showFR _ (UnsupportedLanguage lang)       = " (conflict: requires " ++ showUnsupportedLanguage lang ++ ")"
showFR _ (MissingPkgconfigPackage pn vr)  = " (conflict: pkg-config package " ++ prettyShow pn ++ prettyShow vr ++ ", not found in the pkg-config database)"
showFR _ (MissingPkgconfigProgram pn vr)  = " (pkg-config package " ++ prettyShow pn ++ prettyShow vr ++ " is needed but no pkg-config executable was found or querying it failed)"
showFR _ (NewPackageDoesNotMatchExistingConstraint d) = " (conflict: " ++ showConflictingDep d ++ ")"
showFR _ (ConflictingConstraints d1 d2)   = " (conflict: " ++ L.intercalate ", " (L.map showConflictingDep [d1, d2]) ++ ")"
showFR _ (NewPackageIsMissingRequiredComponent comp dr) = " (does not contain " ++ showExposedComponent comp ++ ", which is required by " ++ showDependencyReason dr ++ ")"
showFR _ (NewPackageHasPrivateRequiredComponent comp dr) = " (" ++ showExposedComponent comp ++ " is private, but it is required by " ++ showDependencyReason dr ++ ")"
showFR _ (NewPackageHasUnbuildableRequiredComponent comp dr) = " (" ++ showExposedComponent comp ++ " is not buildable in the current environment, but it is required by " ++ showDependencyReason dr ++ ")"
showFR _ (PackageRequiresMissingComponent qpn comp) = " (requires " ++ showExposedComponent comp ++ " from " ++ showQPN qpn ++ ", but the component does not exist)"
showFR _ (PackageRequiresPrivateComponent qpn comp) = " (requires " ++ showExposedComponent comp ++ " from " ++ showQPN qpn ++ ", but the component is private)"
showFR _ (PackageRequiresUnbuildableComponent qpn comp) = " (requires " ++ showExposedComponent comp ++ " from " ++ showQPN qpn ++ ", but the component is not buildable in the current environment)"
showFR _ CannotReinstall                  = " (avoiding to reinstall a package with same version but new dependencies)"
showFR _ NotExplicit                      = " (not a user-provided goal nor mentioned as a constraint, but reject-unconstrained-dependencies was set)"
showFR _ Shadowed                         = " (shadowed by another installed package with same version)"
showFR _ (Broken u)                       = " (package is broken, missing dependency " ++ prettyShow u ++ ")"
showFR _ UnknownPackage                   = " (unknown package)"
showFR _ (GlobalConstraintVersion vr (ConstraintSourceProjectConfig pc)) = '\n' : (render . nest 6 $ docProjectConfigPathFailReason vr pc)
showFR _ (GlobalConstraintVersion vr src) = " (" ++ constraintSource src ++ " requires " ++ prettyShow vr ++ ")"
showFR _ (GlobalConstraintInstalled src)  = " (" ++ constraintSource src ++ " requires installed instance)"
showFR _ (GlobalConstraintInstalledSpecificUnitId unitId src)  = " (" ++ constraintSource src ++ " requires installed instance with unit id " ++ prettyShow unitId ++ ")"
showFR _ (GlobalConstraintSource src)     = " (" ++ constraintSource src ++ " requires source instance)"
showFR _ (GlobalConstraintFlag src)       = " (" ++ constraintSource src ++ " requires opposite flag selection)"
showFR _ ManualFlag                       = " (manual flag can only be changed explicitly)"
showFR c Backjump                         = " (backjumping, conflict set: " ++ showConflictSet c ++ ")"
showFR _ MultipleInstances                = " (multiple instances)"
showFR c (DependenciesNotLinked msg)      = " (dependencies not linked: " ++ msg ++ "; conflict set: " ++ showConflictSet c ++ ")"
showFR c CyclicDependencies               = " (cyclic dependencies; conflict set: " ++ showConflictSet c ++ ")"
showFR _ (UnsupportedSpecVer ver)         = " (unsupported spec-version " ++ prettyShow ver ++ ")"
-- The following are internal failures. They should not occur. In the
-- interest of not crashing unnecessarily, we still just print an error
-- message though.
showFR _ (MalformedFlagChoice qfn)        = " (INTERNAL ERROR: MALFORMED FLAG CHOICE: " ++ showQFN qfn ++ ")"
showFR _ (MalformedStanzaChoice qsn)      = " (INTERNAL ERROR: MALFORMED STANZA CHOICE: " ++ showQSN qsn ++ ")"
showFR _ EmptyGoalChoice                  = " (INTERNAL ERROR: EMPTY GOAL CHOICE)"

showExposedComponent :: ExposedComponent -> String
showExposedComponent (ExposedLib LMainLibName)       = "library"
showExposedComponent (ExposedLib (LSubLibName name)) = "library '" ++ unUnqualComponentName name ++ "'"
showExposedComponent (ExposedExe name)               = "executable '" ++ unUnqualComponentName name ++ "'"

constraintSource :: ConstraintSource -> String
constraintSource src = "constraint from " ++ showConstraintSource src

showConflictingDep :: ConflictingDep -> String
showConflictingDep (ConflictingDep dr (PkgComponent qpn comp) ci) =
  let DependencyReason qpn' _ _ = dr
      componentStr = case comp of
                       ExposedExe exe               -> " (exe " ++ unUnqualComponentName exe ++ ")"
                       ExposedLib LMainLibName      -> ""
                       ExposedLib (LSubLibName lib) -> " (lib " ++ unUnqualComponentName lib ++ ")"
  in case ci of
       Fixed i        -> (if qpn /= qpn' then showDependencyReason dr ++ " => " else "") ++
                         showQPN qpn ++ componentStr ++ "==" ++ showI i
       Constrained vr -> showDependencyReason dr ++ " => " ++ showQPN qpn ++
                         componentStr ++ showVR vr

-- $setup
-- >>> import Distribution.Solver.Modular.Package
-- >>> import Distribution.Solver.Types.PackagePath
-- >>> import Distribution.Types.PackageName
-- >>> import Distribution.Types.Version
-- >>> import Distribution.Types.UnitId
-- >>> let foobarPN = PackagePath DefaultNamespace QualToplevel
-- >>> let bazquxPN = PackagePath (Independent $ mkPackageName "bazqux") QualToplevel
-- >>> let foobarQPN = Q foobarPN (mkPackageName "foo-bar")
-- >>> let v0 = POption (I (mkVersion [0]) InRepo) Nothing
-- >>> let v1 = POption (I (mkVersion [1]) InRepo) Nothing
-- >>> let i0 = POption (I (mkVersion [0]) (Inst $ mkUnitId "foo-bar-0-inplace")) Nothing
-- >>> let i1 = POption (I (mkVersion [1]) (Inst $ mkUnitId "foo-bar-1-inplace")) Nothing
-- >>> let k1 = POption (I (mkVersion [1]) InRepo) (Just bazquxPN)
-- >>> let k2 = POption (I (mkVersion [2]) InRepo) (Just bazquxPN)
