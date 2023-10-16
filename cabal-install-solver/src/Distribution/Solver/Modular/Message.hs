{-# LANGUAGE BangPatterns #-}

module Distribution.Solver.Modular.Message (
    Message(..),
    SolverTrace(..),
    groupMessages,
  ) where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (catMaybes, mapMaybe)
import Prelude hiding (pi)

import Distribution.Pretty (prettyShow) -- from Cabal

import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.MessageUtils
         (showUnsupportedExtension, showUnsupportedLanguage)
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
         ( FailReason(..), POption(..), ConflictingDep(..) )
import Distribution.Solver.Modular.Version
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Progress
import Distribution.Types.LibraryName
import Distribution.Types.UnqualComponentName

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

data Log
  = PackageGoal QPN QGoalReason
  | RejectF QFN Bool ConflictSet FailReason
  | RejectS QSN Bool ConflictSet FailReason
  | Skipping' (Set CS.Conflict)
  | TryingF QFN Bool
  | TryingP QPN POption (Maybe (GoalReason QPN))
  | TryingS QSN Bool
  | RejectMany QPN [POption] ConflictSet FailReason
  | SkipMany QPN [POption] (Set CS.Conflict)
  | UnknownPackage' QPN (GoalReason QPN)
  | SuccessMsg
  | FailureMsg ConflictSet FailReason

data AtLevel a = AtLevel Int a

type Trace = AtLevel Log

data SolverTrace = SolverTrace Trace | ErrorMsg String

instance Show SolverTrace where
    show (SolverTrace i) = displayMessageAtLevel i
    show (ErrorMsg s) = show s

instance Show Log where
    show = displayMessage

displayMessageAtLevel :: Trace -> String
displayMessageAtLevel (AtLevel l msg) =
  let s = show l
  in  "[" ++ replicate (3 - length s) '_' ++ s ++ "] " ++ displayMessage msg

displayMessage :: Log -> String
displayMessage (PackageGoal qpn gr) = "next goal: " ++ showQPN qpn ++ showGR gr
displayMessage (RejectF qfn b c fr) = "rejecting: " ++ showQFNBool qfn b ++ showFR c fr
displayMessage (RejectS qsn b c fr) = "rejecting: " ++ showQSNBool qsn b ++ showFR c fr
displayMessage (Skipping' cs) = showConflicts cs
displayMessage (TryingF qfn b) = "trying: " ++ showQFNBool qfn b
displayMessage (TryingP qpn i mgr) = "trying: " ++ showQPNPOpt qpn i ++ maybe "" showGR mgr
displayMessage (TryingS qsn b) = "trying: " ++ showQSNBool qsn b
displayMessage (UnknownPackage' qpn gr) = "unknown package" ++ showQPN qpn ++ showGR gr
displayMessage SuccessMsg = "done"
displayMessage (FailureMsg c fr) = "fail: " ++ showFR c fr
displayMessage (SkipMany _ _ cs) = "skipping: " ++ showConflicts cs
-- TODO: Instead of displaying `aeson-1.0.2.1, aeson-1.0.2.0, aeson-1.0.1.0, ...`,
-- the following line aim to display `aeson: 1.0.2.1, 1.0.2.0, 1.0.1.0, ...`.
--
-- displayMessage (RejectMany qpn is c fr) = "rejecting: " ++ fmtPkgsGroupedByName (map (showQPNPOpt qpn) (reverse is)) ++ showFR c fr
displayMessage (RejectMany qpn is c fr) = "rejecting: " ++ L.intercalate ", " (map (showQPNPOpt qpn) (reverse is)) ++ showFR c fr

-- TODO: This function should take as input the Index? So even without calling the solver, We can say things as
-- "There is no version in the Hackage index that match the given constraints".
--
-- Alternatively, by passing this to the solver, we could get a more semantic output like:
-- `all versions of aeson available are in conflict with ...`. Isn't already what `tryToMinimizeConflictSet` is doing?
-- fmtPkgsGroupedByName :: [String] -> String
-- fmtPkgsGroupedByName pkgs = L.intercalate " " $ fmtPkgGroup (groupByName pkgs)
--   where
--     groupByName :: [String] -> Map.Map String [String]
--     groupByName = foldr f Map.empty
--       where
--         f versionString m = let (pkg, ver) = splitOnLastHyphen versionString
--                             in Map.insertWith (++) pkg [ver] m
--         -- FIXME: This is not a very robust way to split the package name and version.
--         -- I should rather retrieve the package name and version from the QPN ...
--         splitOnLastHyphen :: String -> (String, String)
--         splitOnLastHyphen s =
--             case reverse (L.elemIndices '-' s) of
--                 (x:_)  -> (take x s, drop (x + 1) s)
--                 _ -> error "splitOnLastHyphen: no hyphen found"

--     fmtPkgGroup :: Map.Map String [String] -> [String]
--     fmtPkgGroup = map formatEntry . Map.toList
--       where
--         formatEntry (pkg, versions) = pkg ++ ": " ++ L.intercalate ", " versions

-- | Transforms the structured message type to actual messages (strings).
--
-- The log contains level numbers, which are useful for any trace that involves
-- backtracking, because only the level numbers will allow to keep track of
-- backjumps.
groupMessages :: Progress Message a b -> Progress SolverTrace a b
groupMessages = go 0
  where
    -- 'go' increments the level for a recursive call when it encounters
    -- 'TryP', 'TryF', or 'TryS' and decrements the level when it encounters 'Leave'.
    go :: Int -> Progress Message a b -> Progress SolverTrace a b
    go !_ (Done x)                           = Done x
    go !_ (Fail x)                           = Fail x

    -- complex patterns
    go !l (Step (TryP qpn i) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        goPReject l qpn [i] c fr ms

    go !l (Step (TryP qpn i) (Step Enter (Step (Skip conflicts) (Step Leave ms)))) =
        goPSkip l qpn [i] conflicts ms

    go !l (Step (TryF qfn b) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        Step (SolverTrace $ AtLevel l $ (RejectF qfn b c fr)) (go l ms)

    go !l (Step (TryS qsn b) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        Step (SolverTrace $ AtLevel l $ (RejectS qsn b c fr)) (go l ms)

    -- "Trying ..." message when a new goal is started
    go !l (Step (Next (Goal (P _  ) gr)) (Step (TryP qpn' i) ms@(Step Enter (Step (Next _) _)))) =
        Step (SolverTrace $ AtLevel l $ (TryingP qpn' i (Just gr))) (go l ms)

    go !l (Step (Next (Goal (P qpn) gr)) (Step (Failure _c UnknownPackage) ms)) =
        Step (SolverTrace $ AtLevel l $ (UnknownPackage' qpn gr)) (go l ms)

    -- standard display
    go !l (Step Enter                    ms) = go (l+1) ms
    go !l (Step Leave                    ms) = go (l-1) ms

    go !l (Step (TryP qpn i)             ms) = Step (SolverTrace $ AtLevel l $ (TryingP qpn i Nothing)) (go l ms)
    go !l (Step (TryF qfn b)             ms) = Step (SolverTrace $ AtLevel l $ (TryingF qfn b)) (go l ms)
    go !l (Step (TryS qsn b)             ms) = Step (SolverTrace $ AtLevel l $ (TryingS qsn b)) (go l ms)
    go !l (Step (Next (Goal (P qpn) gr)) ms) = Step (SolverTrace $ AtLevel l $ (PackageGoal qpn gr)) (go l ms)
    go !l (Step (Next _)                 ms) = go l ms -- ignore flag goals in the log

    -- 'Skip' should always be handled by 'goPSkip' in the case above.
    go !l (Step (Skip conflicts)         ms) = Step (SolverTrace $ AtLevel l $ (Skipping' conflicts)) (go l ms)
    go !l (Step (Success)                ms) = Step (SolverTrace $ AtLevel l $ SuccessMsg) (go l ms)
    go !l (Step (Failure c fr)           ms) = Step (SolverTrace $ AtLevel l $ (FailureMsg c fr)) (go l ms)

    -- special handler for many subsequent package rejections
    goPReject :: Int
              -> QPN
              -> [POption]
              -> ConflictSet
              -> FailReason
              -> Progress Message a b
              -> Progress SolverTrace a b
    goPReject l qpn is c fr (Step (TryP qpn' i) (Step Enter (Step (Failure _ fr') (Step Leave ms))))
      | qpn == qpn' && fr == fr' =
        goPReject l qpn (i : is) c fr ms
    goPReject l qpn is c fr ms =
        Step (SolverTrace $ AtLevel l $ (RejectMany qpn is c fr)) (go l ms)

    -- Handle many subsequent skipped package instances.
    goPSkip :: Int
            -> QPN
            -> [POption]
            -> Set CS.Conflict
            -> Progress Message a b
            -> Progress SolverTrace a b
    goPSkip l qpn is conflicts (Step (TryP qpn' i) (Step Enter (Step (Skip conflicts') (Step Leave ms))))
      | qpn == qpn' && conflicts == conflicts' = goPSkip l qpn (i : is) conflicts ms
    goPSkip l qpn is conflicts ms =
       Step (SolverTrace $ AtLevel l $ (SkipMany qpn is conflicts)) (go l ms)

-- | Display the set of 'Conflicts' for a skipped package version.
showConflicts :: Set CS.Conflict -> String
showConflicts conflicts =
    " (has the same characteristics that caused the previous version to fail: "
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

showQPNPOpt :: QPN -> POption -> String
showQPNPOpt qpn@(Q _pp pn) (POption i linkedTo) =
  case linkedTo of
    Nothing  -> showPI (PI qpn i) -- Consistent with prior to POption
    Just pp' -> showQPN qpn ++ "~>" ++ showPI (PI (Q pp' pn) i)

showGR :: QGoalReason -> String
showGR UserGoal            = " (user goal)"
showGR (DependencyGoal dr) = " (dependency of " ++ showDependencyReason dr ++ ")"

showFR :: ConflictSet -> FailReason -> String
showFR _ (UnsupportedExtension ext)       = " (conflict: requires " ++ showUnsupportedExtension ext ++ ")"
showFR _ (UnsupportedLanguage lang)       = " (conflict: requires " ++ showUnsupportedLanguage lang ++ ")"
showFR _ (MissingPkgconfigPackage pn vr)  = " (conflict: pkg-config package " ++ prettyShow pn ++ prettyShow vr ++ ", not found in the pkg-config database)"
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
showFR _ (GlobalConstraintVersion vr src) = " (" ++ constraintSource src ++ " requires " ++ prettyShow vr ++ ")"
showFR _ (GlobalConstraintInstalled src)  = " (" ++ constraintSource src ++ " requires installed instance)"
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
