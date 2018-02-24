{-# LANGUAGE BangPatterns #-}

module Distribution.Solver.Modular.Message (
    Message(..),
    showMessages
  ) where

import qualified Data.List as L
import Prelude hiding (pi)

import Distribution.Text -- from Cabal

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
         ( FailReason(..), POption(..), ConflictingDep(..) )
import Distribution.Solver.Modular.Version
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Progress
import Distribution.Types.UnqualComponentName

data Message =
    Enter           -- ^ increase indentation level
  | Leave           -- ^ decrease indentation level
  | TryP QPN POption
  | TryF QFN Bool
  | TryS QSN Bool
  | Next (Goal QPN)
  | Success
  | Failure ConflictSet FailReason

-- | Transforms the structured message type to actual messages (strings).
--
-- The log contains level numbers, which are useful for any trace that involves
-- backtracking, because only the level numbers will allow to keep track of
-- backjumps.
showMessages :: Progress Message a b -> Progress String a b
showMessages = go 0
  where
    -- 'go' increments the level for a recursive call when it encounters
    -- 'TryP', 'TryF', or 'TryS' and decrements the level when it encounters 'Leave'.
    go :: Int -> Progress Message a b -> Progress String a b
    go !_ (Done x)                           = Done x
    go !_ (Fail x)                           = Fail x
    -- complex patterns
    go !l (Step (TryP qpn i) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        goPReject l qpn [i] c fr ms
    go !l (Step (TryF qfn b) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        (atLevel l $ "rejecting: " ++ showQFNBool qfn b ++ showFR c fr) (go l ms)
    go !l (Step (TryS qsn b) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        (atLevel l $ "rejecting: " ++ showQSNBool qsn b ++ showFR c fr) (go l ms)
    go !l (Step (Next (Goal (P _  ) gr)) (Step (TryP qpn' i) ms@(Step Enter (Step (Next _) _)))) =
        (atLevel l $ "trying: " ++ showQPNPOpt qpn' i ++ showGR gr) (go l ms)
    go !l (Step (Next (Goal (P qpn) gr)) ms@(Step (Failure _c Backjump) _)) =
        (atLevel l $ "unknown package: " ++ showQPN qpn ++ showGR gr) $ go l ms
    go !l (Step (Next (Goal (P qpn) gr)) (Step (Failure c fr) ms)) =
        (atLevel l $ showPackageGoal qpn gr) $ (atLevel l $ showFailure c fr) (go l ms)
    -- standard display
    go !l (Step Enter                    ms) = go (l+1) ms
    go !l (Step Leave                    ms) = go (l-1) ms
    go !l (Step (TryP qpn i)             ms) = (atLevel l $ "trying: " ++ showQPNPOpt qpn i) (go l ms)
    go !l (Step (TryF qfn b)             ms) = (atLevel l $ "trying: " ++ showQFNBool qfn b) (go l ms)
    go !l (Step (TryS qsn b)             ms) = (atLevel l $ "trying: " ++ showQSNBool qsn b) (go l ms)
    go !l (Step (Next (Goal (P qpn) gr)) ms) = (atLevel l $ showPackageGoal qpn gr) (go l ms)
    go !l (Step (Next _)                 ms) = go l     ms -- ignore flag goals in the log
    go !l (Step (Success)                ms) = (atLevel l $ "done") (go l ms)
    go !l (Step (Failure c fr)           ms) = (atLevel l $ showFailure c fr) (go l ms)

    showPackageGoal :: QPN -> QGoalReason -> String
    showPackageGoal qpn gr = "next goal: " ++ showQPN qpn ++ showGR gr

    showFailure :: ConflictSet -> FailReason -> String
    showFailure c fr = "fail" ++ showFR c fr

    -- special handler for many subsequent package rejections
    goPReject :: Int
              -> QPN
              -> [POption]
              -> ConflictSet
              -> FailReason
              -> Progress Message a b
              -> Progress String a b
    goPReject l qpn is c fr (Step (TryP qpn' i) (Step Enter (Step (Failure _ fr') (Step Leave ms))))
      | qpn == qpn' && fr == fr' = goPReject l qpn (i : is) c fr ms
    goPReject l qpn is c fr ms =
        (atLevel l $ "rejecting: " ++ L.intercalate ", " (map (showQPNPOpt qpn) (reverse is)) ++ showFR c fr) (go l ms)

    -- write a message with the current level number
    atLevel :: Int -> String -> Progress String a b -> Progress String a b
    atLevel l x xs =
      let s = show l
      in  Step ("[" ++ replicate (3 - length s) '_' ++ s ++ "] " ++ x) xs

showQPNPOpt :: QPN -> POption -> String
showQPNPOpt qpn@(Q _pp pn) (POption i linkedTo) =
  case linkedTo of
    Nothing  -> showPI (PI qpn i) -- Consistent with prior to POption
    Just pp' -> showQPN qpn ++ "~>" ++ showPI (PI (Q pp' pn) i)

showGR :: QGoalReason -> String
showGR UserGoal            = " (user goal)"
showGR (DependencyGoal dr) = " (dependency of " ++ showDependencyReason dr ++ ")"

showFR :: ConflictSet -> FailReason -> String
showFR _ (UnsupportedExtension ext)       = " (conflict: requires " ++ display ext ++ ")"
showFR _ (UnsupportedLanguage lang)       = " (conflict: requires " ++ display lang ++ ")"
showFR _ (MissingPkgconfigPackage pn vr)  = " (conflict: pkg-config package " ++ display pn ++ display vr ++ ", not found in the pkg-config database)"
showFR _ (NewPackageDoesNotMatchExistingConstraint d) = " (conflict: " ++ showConflictingDep d ++ ")"
showFR _ (ConflictingConstraints d1 d2)   = " (conflict: " ++ L.intercalate ", " (L.map showConflictingDep [d1, d2]) ++ ")"
showFR _ (NewPackageIsMissingRequiredExe exe dr) = " (does not contain executable " ++ unUnqualComponentName exe ++ ", which is required by " ++ showDependencyReason dr ++ ")"
showFR _ (PackageRequiresMissingExe qpn exe) = " (requires executable " ++ unUnqualComponentName exe ++ " from " ++ showQPN qpn ++ ", but the executable does not exist)"
showFR _ CannotInstall                    = " (only already installed instances can be used)"
showFR _ CannotReinstall                  = " (avoiding to reinstall a package with same version but new dependencies)"
showFR _ Shadowed                         = " (shadowed by another installed package with same version)"
showFR _ Broken                           = " (package is broken)"
showFR _ (GlobalConstraintVersion vr src) = " (" ++ constraintSource src ++ " requires " ++ display vr ++ ")"
showFR _ (GlobalConstraintInstalled src)  = " (" ++ constraintSource src ++ " requires installed instance)"
showFR _ (GlobalConstraintSource src)     = " (" ++ constraintSource src ++ " requires source instance)"
showFR _ (GlobalConstraintFlag src)       = " (" ++ constraintSource src ++ " requires opposite flag selection)"
showFR _ ManualFlag                       = " (manual flag can only be changed explicitly)"
showFR c Backjump                         = " (backjumping, conflict set: " ++ showConflictSet c ++ ")"
showFR _ MultipleInstances                = " (multiple instances)"
showFR c (DependenciesNotLinked msg)      = " (dependencies not linked: " ++ msg ++ "; conflict set: " ++ showConflictSet c ++ ")"
showFR c CyclicDependencies               = " (cyclic dependencies; conflict set: " ++ showConflictSet c ++ ")"
showFR _ (UnsupportedSpecVer ver)         = " (unsupported spec-version " ++ display ver ++ ")"
-- The following are internal failures. They should not occur. In the
-- interest of not crashing unnecessarily, we still just print an error
-- message though.
showFR _ (MalformedFlagChoice qfn)        = " (INTERNAL ERROR: MALFORMED FLAG CHOICE: " ++ showQFN qfn ++ ")"
showFR _ (MalformedStanzaChoice qsn)      = " (INTERNAL ERROR: MALFORMED STANZA CHOICE: " ++ showQSN qsn ++ ")"
showFR _ EmptyGoalChoice                  = " (INTERNAL ERROR: EMPTY GOAL CHOICE)"

constraintSource :: ConstraintSource -> String
constraintSource src = "constraint from " ++ showConstraintSource src

showConflictingDep :: ConflictingDep -> String
showConflictingDep (ConflictingDep dr mExe qpn ci) =
  let DependencyReason qpn' _ _ = dr
      exeStr = case mExe of
                 Just exe -> " (exe " ++ unUnqualComponentName exe ++ ")"
                 Nothing  -> ""
  in case ci of
       Fixed i        -> (if qpn /= qpn' then showDependencyReason dr ++ " => " else "") ++
                         showQPN qpn ++ exeStr ++ "==" ++ showI i
       Constrained vr -> showDependencyReason dr ++ " => " ++ showQPN qpn ++
                         exeStr ++ showVR vr
