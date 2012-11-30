module Distribution.Client.Dependency.Modular.Message where

import qualified Data.List as L
import Prelude hiding (pi)

import Distribution.Text -- from Cabal

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree

data Message =
    Enter           -- ^ increase indentation level
  | Leave           -- ^ decrease indentation level
  | TryP (PI QPN)
  | TryF QFN Bool
  | TryS QSN Bool
  | Next (Goal QPN)
  | Success
  | Failure (ConflictSet QPN) FailReason

-- | Transforms the structured message type to actual messages (strings).
--
-- Takes an additional relevance predicate. The predicate gets a stack of goal
-- variables and can decide whether messages regarding these goals are relevant.
-- You can plug in 'const True' if you're interested in a full trace. If you
-- want a slice of the trace concerning a particular conflict set, then plug in
-- a predicate returning 'True' on the empty stack and if the head is in the
-- conflict set.
--
-- The second argument indicates if the level numbers should be shown. This is
-- recommended for any trace that involves backtracking, because only the level
-- numbers will allow to keep track of backjumps.
showMessages :: ([Var QPN] -> Bool) -> Bool -> [Message] -> [String]
showMessages p sl = go [] 0
  where
    go :: [Var QPN] -> Int -> [Message] -> [String]
    go _ _ []                            = []
    -- complex patterns
    go v l (TryP (PI qpn i) : Enter : Failure c fr : Leave : ms) = goPReject v l qpn [i] c fr ms
    go v l (TryF qfn b : Enter : Failure c fr : Leave : ms) = (atLevel (F qfn : v) l $ "rejecting: " ++ showQFNBool qfn b ++ showFR c fr) (go v l ms)
    go v l (TryS qsn b : Enter : Failure c fr : Leave : ms) = (atLevel (S qsn : v) l $ "rejecting: " ++ showQSNBool qsn b ++ showFR c fr) (go v l ms)
    go v l (Next (Goal (P qpn) gr) : TryP pi : ms@(Enter : Next _ : _)) = (atLevel (P qpn : v) l $ "trying: " ++ showPI pi ++ showGRs gr) (go (P qpn : v) l ms)
    go v l (Failure c Backjump : ms@(Leave : Failure c' Backjump : _)) | c == c' = go v l ms
    -- standard display
    go v l (Enter                  : ms) = go v          (l+1) ms
    go v l (Leave                  : ms) = go (drop 1 v) (l-1) ms
    go v l (TryP pi@(PI qpn _)     : ms) = (atLevel (P qpn : v) l $ "trying: " ++ showPI pi) (go (P qpn : v) l ms)
    go v l (TryF qfn b             : ms) = (atLevel (F qfn : v) l $ "trying: " ++ showQFNBool qfn b) (go (F qfn : v) l ms)
    go v l (TryS qsn b             : ms) = (atLevel (S qsn : v) l $ "trying: " ++ showQSNBool qsn b) (go (S qsn : v) l ms)
    go v l (Next (Goal (P qpn) gr) : ms) = (atLevel (P qpn : v) l $ "next goal: " ++ showQPN qpn ++ showGRs gr) (go v l ms)
    go v l (Next _                 : ms) = go v l ms -- ignore flag goals in the log
    go v l (Success                : ms) = (atLevel v l $ "done") (go v l ms)
    go v l (Failure c fr           : ms) = (atLevel v l $ "fail" ++ showFR c fr) (go v l ms)

    -- special handler for many subsequent package rejections
    goPReject :: [Var QPN] -> Int -> QPN -> [I] -> ConflictSet QPN -> FailReason -> [Message] -> [String]
    goPReject v l qpn is c fr (TryP (PI qpn' i) : Enter : Failure _ fr' : Leave : ms) | qpn == qpn' && fr == fr' = goPReject v l qpn (i : is) c fr ms
    goPReject v l qpn is c fr ms = (atLevel (P qpn : v) l $ "rejecting: " ++ showQPN qpn ++ "-" ++ L.intercalate ", " (map showI (reverse is)) ++ showFR c fr) (go v l ms)

    -- write a message, but only if it's relevant; we can also enable or disable the display of the current level
    atLevel v l x xs
      | sl && p v = let s = show l
                    in  ("[" ++ replicate (3 - length s) '_' ++ s ++ "] " ++ x) : xs
      | p v       = x : xs
      | otherwise = xs

showGRs :: QGoalReasonChain -> String
showGRs (gr : _) = showGR gr
showGRs []       = ""

showGR :: GoalReason QPN -> String
showGR UserGoal            = " (user goal)"
showGR (PDependency pi)    = " (dependency of " ++ showPI pi            ++ ")"
showGR (FDependency qfn b) = " (dependency of " ++ showQFNBool qfn b    ++ ")"
showGR (SDependency qsn)   = " (dependency of " ++ showQSNBool qsn True ++ ")"

showFR :: ConflictSet QPN -> FailReason -> String
showFR _ InconsistentInitialConstraints = " (inconsistent initial constraints)"
showFR _ (Conflicting ds)               = " (conflict: " ++ L.intercalate ", " (map showDep ds) ++ ")"
showFR _ CannotInstall                  = " (only already installed instances can be used)"
showFR _ CannotReinstall                = " (avoiding to reinstall a package with same version but new dependencies)"
showFR _ Shadowed                       = " (shadowed by another installed package with same version)"
showFR _ Broken                         = " (package is broken)"
showFR _ (GlobalConstraintVersion vr)   = " (global constraint requires " ++ display vr ++ ")"
showFR _ GlobalConstraintInstalled      = " (global constraint requires installed instance)"
showFR _ GlobalConstraintSource         = " (global constraint requires source instance)"
showFR _ GlobalConstraintFlag           = " (global constraint requires opposite flag selection)"
showFR _ ManualFlag                     = " (manual flag can only be changed explicitly)"
showFR _ (BuildFailureNotInIndex pn)    = " (unknown package: " ++ display pn ++ ")"
showFR c Backjump                       = " (backjumping, conflict set: " ++ showCS c ++ ")"
-- The following are internal failures. They should not occur. In the
-- interest of not crashing unnecessarily, we still just print an error
-- message though.
showFR _ (MalformedFlagChoice qfn)      = " (INTERNAL ERROR: MALFORMED FLAG CHOICE: " ++ showQFN qfn ++ ")"
showFR _ (MalformedStanzaChoice qsn)    = " (INTERNAL ERROR: MALFORMED STANZA CHOICE: " ++ showQSN qsn ++ ")"
showFR _ EmptyGoalChoice                = " (INTERNAL ERROR: EMPTY GOAL CHOICE)"
