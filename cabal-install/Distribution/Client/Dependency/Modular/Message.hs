module Distribution.Client.Dependency.Modular.Message where

import qualified Data.Set as S
import Prelude hiding (pi)

import Distribution.Text -- from Cabal

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree

data Message =
    Enter         -- ^ increase indentation level
  | Leave         -- ^ decrease indentation level
  | TryP (PI QPN)
  | TryF QFN Bool
  | Next Goal
  | Success
  | Failure (ConflictSet QPN) FailReason

showMessages :: [Message] -> [String]
showMessages = go 0
  where
    go :: Int -> [Message] -> [String]
    go _ []                  = []
    -- complex patterns
    go l (TryP pi : Enter : Failure c fr : Leave : ms) = (atLevel l $ "rejecting: " ++ showPI pi ++ showFR c fr) : go l ms
    go l (TryF qfn b : Enter : Failure c fr : Leave : ms) = (atLevel l $ "rejecting: " ++ showQFNBool qfn b ++ showFR c fr) : go l ms
    go l (Next (Goal (Simple (Dep _ _)) gr) : TryP pi : ms@(Enter : Next _ : _)) = (atLevel l $ "trying: " ++ showPI pi ++ showGRs gr) : go l ms
    go l (Failure c Backjump : ms@(Leave : Failure c' Backjump : _)) | c == c' = go l ms
    -- standard display
    go l (Enter        : ms) = go (l+1) ms
    go l (Leave        : ms) = go (l-1) ms
    go l (TryP pi      : ms) = (atLevel l $ "trying: " ++ showPI pi) : go l ms
    go l (TryF qfn b   : ms) = (atLevel l $ "trying: " ++ showQFNBool qfn b) : go l ms
    go l (Next (Goal (Simple (Dep qpn _)) gr) : ms) = (atLevel l $ "next goal: " ++ showQPN qpn ++ showGRs gr) : go l ms
    go l (Next _       : ms) = go l ms -- ignore flag goals in the log
    go l (Success      : ms) = (atLevel l $ "done") : go l ms
    go l (Failure c fr : ms) = (atLevel l $ "fail" ++ showFR c fr) : go l ms

    atLevel l x = let s = show l
                  in  "[" ++ replicate (3 - length s) '_' ++ s ++ "] " ++ x

showGRs :: GoalReasons -> String
showGRs (gr : _) = showGR gr
showGRs []       = ""

showGR :: GoalReason -> String
showGR UserGoal            = " (user goal)"
showGR (PDependency pi)    = " (dependency of " ++ showPI pi         ++ ")"
showGR (FDependency qfn b) = " (dependency of " ++ showQFNBool qfn b ++ ")"

showFR :: ConflictSet QPN -> FailReason -> String
showFR _ InconsistentInitialConstraints = " (inconsistent initial constraints)"
showFR _ (Conflicting d)                = " (conflicts with " ++ showDep d ++ ")"
showFR _ ConflictingFlag                = " (conflicts with previous choice of same flag)"
showFR _ CannotInstall                  = " (only already installed versions can be used)"
showFR _ CannotReinstall                = " (avoiding to reinstall a package with same version but new dependencies)"
showFR _ (GlobalConstraintVersion vr)   = " (global constraint requires " ++ display vr ++ ")"
showFR _ GlobalConstraintInstalled      = " (global constraint requires installed instance)"
showFR _ GlobalConstraintSource         = " (global constraint requires source instance)"
showFR _ GlobalConstraintFlag           = " (global constraint requires opposite flag selection)"
showFR c Backjump                       = " (backjumping, conflict set: " ++ showCS c ++ ")"
showFR _ (BuildFailureNotInIndex pn)    = " (BUILD FAILURE: NOT IN INDEX: " ++ display pn ++ ")"
showFR _ EmptyGoalChoice                = " (INTERNAL ERROR: EMPTY GOAL CHOICE)"
