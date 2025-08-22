{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Solver.Modular
         ( modularResolver, SolverConfig(..), PruneAfterFirstSuccess(..) ) where

-- Here, we try to map between the external cabal-install solver
-- interface and the internal interface that the solver actually
-- expects. There are a number of type conversions to perform: we
-- have to convert the package indices to the uniform index used
-- by the solver; we also have to convert the initial constraints;
-- and finally, we have to convert back the resulting install
-- plan.

import Prelude ()
import Distribution.Solver.Compat.Prelude

import qualified Data.Map as M
import Data.Set (isSubsetOf)
import Distribution.Compat.Graph
         ( IsNode(..) )
import Distribution.Compiler
         ( CompilerInfo )
import Distribution.Solver.Modular.Assignment
         ( Assignment, toCPs )
import Distribution.Solver.Modular.ConfiguredConversion
         ( convCP )
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.Dependency
    ( Var(..),
      showVar,
      ConflictMap,
      ConflictSet,
      showConflictSet,
      RevDepMap )
import Distribution.Solver.Modular.Flag ( SN(SN), FN(FN) )
import Distribution.Solver.Modular.Index ( Index )
import Distribution.Solver.Modular.IndexConversion
         ( convPIs )
import Distribution.Solver.Modular.Log
         ( SolverFailure(..), displayLogMessages )
import Distribution.Solver.Modular.Package
         ( PN )
import Distribution.Solver.Modular.RetryLog
    ( RetryLog,
      toProgress,
      fromProgress,
      retry,
      failWith,
      continueWith )
import Distribution.Solver.Modular.Solver
         ( SolverConfig(..), PruneAfterFirstSuccess(..), solve )
import Distribution.Solver.Types.DependencyResolver
    ( DependencyResolver )
import Distribution.Solver.Types.LabeledPackageConstraint
    ( LabeledPackageConstraint, unlabelPackageConstraint )
import Distribution.Solver.Types.PackageConstraint
    ( PackageConstraint(..), scopeToPackageName )
import Distribution.Solver.Types.PackagePath ( QPN )
import Distribution.Solver.Types.PackagePreferences
    ( PackagePreferences )
import Distribution.Solver.Types.PkgConfigDb
         ( PkgConfigDb )
import Distribution.Solver.Types.Progress
    ( Progress(..), foldProgress )
import Distribution.Solver.Types.SummarizedMessage
    ( SummarizedMessage(StringMsg) )
import Distribution.Solver.Types.Variable ( Variable(..) )
import Distribution.System
         ( Platform(..) )
import Distribution.Simple.Setup
         ( BooleanFlag(..) )
import Distribution.Simple.Utils
    ( ordNubBy )
import Distribution.Verbosity
import Distribution.Solver.Modular.Message ( renderSummarizedMessage )

-- | Ties the two worlds together: classic cabal-install vs. the modular
-- solver. Performs the necessary translations before and after.
modularResolver :: SolverConfig -> DependencyResolver loc
modularResolver sc (Platform arch os) cinfo iidx sidx pkgConfigDB pprefs pcs pns =
  uncurry postprocess <$> -- convert install plan
  solve' sc cinfo idx pkgConfigDB pprefs gcs pns
    where
      -- Indices have to be converted into solver-specific uniform index.
      idx    = convPIs os arch cinfo gcs (shadowPkgs sc) (strongFlags sc) (solveExecutables sc) iidx sidx
      -- Constraints have to be converted into a finite map indexed by PN.
      gcs    = M.fromListWith (++) (map pair pcs)
        where
          pair lpc = (pcName $ unlabelPackageConstraint lpc, [lpc])

      -- Results have to be converted into an install plan. 'convCP' removes
      -- package qualifiers, which means that linked packages become duplicates
      -- and can be removed.
      postprocess a rdm = ordNubBy nodeKey $
                          map (convCP iidx sidx) (toCPs a rdm)

      -- Helper function to extract the PN from a constraint.
      pcName :: PackageConstraint -> PN
      pcName (PackageConstraint scope _) = scopeToPackageName scope

-- | Run 'D.S.Modular.Solver.solve' and then produce a summarized log to display
-- in the error case.
--
-- When there is no solution, we produce the error message by rerunning the
-- solver but making it prefer the goals from the final conflict set from the
-- first run (or a subset of the final conflict set with
-- --minimize-conflict-set). We also set the backjump limit to 0, so that the
-- log stops at the first backjump and is relatively short. Preferring goals
-- from the final conflict set increases the probability that the log to the
-- first backjump contains package, flag, and stanza choices that are relevant
-- to the final failure. The solver shouldn't need to choose any packages that
-- aren't in the final conflict set. (For every variable in the final conflict
-- set, the final conflict set should also contain the variable that introduced
-- that variable. The solver can then follow that chain of variables in reverse
-- order from the user target to the conflict.) However, it is possible that the
-- conflict set contains unnecessary variables.
--
-- Producing an error message when the solver reaches the backjump limit is more
-- complicated. There is no final conflict set, so we create one for the minimal
-- subtree containing the path that the solver took to the first backjump. This
-- conflict set helps explain why the solver reached the backjump limit, because
-- the first backjump contributes to reaching the backjump limit. Additionally,
-- the solver is much more likely to be able to finish traversing this subtree
-- before the backjump limit, since its size is linear (not exponential) in the
-- number of goal choices. We create it by pruning all children after the first
-- successful child under each node in the original tree, so that there is at
-- most one valid choice at each level. Then we use the final conflict set from
-- that run to generate an error message, as in the case where the solver found
-- that there was no solution.
--
-- Using the full log from a rerun of the solver ensures that the log is
-- complete, i.e., it shows the whole chain of dependencies from the user
-- targets to the conflicting packages.
solve' :: SolverConfig
       -> CompilerInfo
       -> Index
       -> Maybe PkgConfigDb
       -> (PN -> PackagePreferences)
       -> Map PN [LabeledPackageConstraint]
       -> Set PN
       -> Progress SummarizedMessage String (Assignment, RevDepMap)
solve' sc cinfo idx pkgConfigDB pprefs gcs pns =
    toProgress $ retry (runSolver printFullLog sc) createErrorMsg
  where
    runSolver :: Bool -> SolverConfig
              -> RetryLog SummarizedMessage SolverFailure (Assignment, RevDepMap)
    runSolver keepLog sc' =
        displayLogMessages keepLog $
        solve sc' cinfo idx pkgConfigDB pprefs gcs pns

    createErrorMsg :: SolverFailure
                   -> RetryLog SummarizedMessage String (Assignment, RevDepMap)
    createErrorMsg failure@(ExhaustiveSearch cs cm) =
      if asBool $ minimizeConflictSet sc
      then continueWith (mkStringMsg $ "Found no solution after exhaustively searching the "
                          ++ "dependency tree. Rerunning the dependency solver "
                          ++ "to minimize the conflict set ({"
                          ++ showConflictSet cs ++ "}).") $
           retry (tryToMinimizeConflictSet (runSolver printFullLog) sc cs cm) $
               \case
                  ExhaustiveSearch cs' cm' ->
                      fromProgress $ Fail $
                          rerunSolverForErrorMsg cs'
                       ++ finalErrorMsg sc (ExhaustiveSearch cs' cm')
                  BackjumpLimitReached ->
                      fromProgress $ Fail $
                          "Reached backjump limit while trying to minimize the "
                       ++ "conflict set to create a better error message. "
                       ++ "Original error message:\n"
                       ++ rerunSolverForErrorMsg cs
                       ++ finalErrorMsg sc failure
      else fromProgress $ Fail $
           rerunSolverForErrorMsg cs ++ finalErrorMsg sc failure
    createErrorMsg failure@BackjumpLimitReached     =
        continueWith
             (mkStringMsg $ "Backjump limit reached. Rerunning dependency solver to generate "
              ++ "a final conflict set for the search tree containing the "
              ++ "first backjump.") $
        retry (runSolver printFullLog sc { pruneAfterFirstSuccess = PruneAfterFirstSuccess True }) $
            \case
               ExhaustiveSearch cs _ ->
                   fromProgress $ Fail $
                   rerunSolverForErrorMsg cs ++ finalErrorMsg sc failure
               BackjumpLimitReached  ->
                   -- This case is possible when the number of goals involved in
                   -- conflicts is greater than the backjump limit.
                   fromProgress $ Fail $ finalErrorMsg sc failure
                    ++ "Failed to generate a summarized dependency solver "
                    ++ "log due to low backjump limit."

    rerunSolverForErrorMsg :: ConflictSet -> String
    rerunSolverForErrorMsg cs =
      let sc' = sc {
                    goalOrder = Just goalOrder'
                  , maxBackjumps = Just 0
                  }

          -- Preferring goals from the conflict set takes precedence over the
          -- original goal order.
          goalOrder' = preferGoalsFromConflictSet cs <> fromMaybe mempty (goalOrder sc)

      in unlines ("Could not resolve dependencies:" : map renderSummarizedMessage (messages (toProgress (runSolver True sc'))))

    printFullLog = solverVerbosity sc >= Verbose

    messages :: Progress step fail done -> [step]
    messages = foldProgress (:) (const []) (const [])

mkStringMsg :: String -> SummarizedMessage
mkStringMsg msg = StringMsg msg

-- | Try to remove variables from the given conflict set to create a minimal
-- conflict set.
--
-- Minimal means that no proper subset of the conflict set is also a conflict
-- set, though there may be other possible conflict sets with fewer variables.
-- This function minimizes the input by trying to remove one variable at a time.
-- It only makes one pass over the variables, so it runs the solver at most N
-- times when given a conflict set of size N. Only one pass is necessary,
-- because every superset of a conflict set is also a conflict set, meaning that
-- failing to remove variable X from a conflict set in one step means that X
-- cannot be removed from any subset of that conflict set in a subsequent step.
--
-- Example steps:
--
-- Start with {A, B, C}.
-- Try to remove A from {A, B, C} and fail.
-- Try to remove B from {A, B, C} and succeed.
-- Try to remove C from {A, C} and fail.
-- Return {A, C}
--
-- This function can fail for two reasons:
--
-- 1. The solver can reach the backjump limit on any run. In this case the
--    returned RetryLog ends with BackjumpLimitReached.
--    TODO: Consider applying the backjump limit to all solver runs combined,
--    instead of each individual run. For example, 10 runs with 10 backjumps
--    each should count as 100 backjumps.
-- 2. Since this function works by rerunning the solver, it is possible for the
--    solver to add new unnecessary variables to the conflict set. This function
--    discards the result from any run that adds new variables to the conflict
--    set, but the end result may not be completely minimized.
tryToMinimizeConflictSet :: forall a . (SolverConfig -> RetryLog SummarizedMessage SolverFailure a)
                         -> SolverConfig
                         -> ConflictSet
                         -> ConflictMap
                         -> RetryLog SummarizedMessage SolverFailure a
tryToMinimizeConflictSet runSolver sc cs cm =
    foldl (\r v -> retryNoSolution r $ tryToRemoveOneVar v)
          (fromProgress $ Fail $ ExhaustiveSearch cs cm)
          (CS.toList cs)
  where
    -- This function runs the solver and makes it prefer goals in the following
    -- order:
    --
    -- 1. variables in 'smallestKnownCS', excluding 'v'
    -- 2. 'v'
    -- 3. all other variables
    --
    -- If 'v' is not necessary, then the solver will find that there is no
    -- solution before starting to solve for 'v', and the new final conflict set
    -- will be very likely to not contain 'v'. If 'v' is necessary, the solver
    -- will most likely need to try solving for 'v' before finding that there is
    -- no solution, and the new final conflict set will still contain 'v'.
    -- However, this method isn't perfect, because it is possible for the solver
    -- to add new unnecessary variables to the conflict set on any run. This
    -- function prevents the conflict set from growing by checking that the new
    -- conflict set is a subset of the old one and falling back to using the old
    -- conflict set when that check fails.
    tryToRemoveOneVar :: Var QPN
                      -> ConflictSet
                      -> ConflictMap
                      -> RetryLog SummarizedMessage SolverFailure a
    tryToRemoveOneVar v smallestKnownCS smallestKnownCM
        -- Check whether v is still present, because it may have already been
        -- removed in a previous solver rerun.
      | not (v `CS.member` smallestKnownCS) =
          fromProgress $ Fail $ ExhaustiveSearch smallestKnownCS smallestKnownCM
      | otherwise =
        continueWith (mkStringMsg $ "Trying to remove variable " ++ varStr ++ " from the "
                      ++ "conflict set.") $
        retry (runSolver sc') $ \case
            err@(ExhaustiveSearch cs' _)
              | CS.toSet cs' `isSubsetOf` CS.toSet smallestKnownCS ->
                  let msg = if not $ CS.member v cs'
                            then "Successfully removed " ++ varStr ++ " from "
                                  ++ "the conflict set."
                            else "Failed to remove " ++ varStr ++ " from the "
                                  ++ "conflict set."
                  in -- Use the new conflict set, even if v wasn't removed,
                     -- because other variables may have been removed.
                     failWith (mkStringMsg $ msg ++ " Continuing with " ++ showCS cs' ++ ".") err
              | otherwise ->
                  failWith (mkStringMsg $ "Failed to find a smaller conflict set. The new "
                             ++ "conflict set is not a subset of the previous "
                             ++ "conflict set: " ++ showCS cs') $
                  ExhaustiveSearch smallestKnownCS smallestKnownCM
            BackjumpLimitReached ->
                failWith (mkStringMsg "Reached backjump limit while minimizing conflict set.")
                         BackjumpLimitReached
      where
        varStr = "\"" ++ showVar v ++ "\""
        showCS cs' = "{" ++ showConflictSet cs' ++ "}"

        sc' = sc { goalOrder = Just goalOrder' }

        goalOrder' =
            preferGoalsFromConflictSet (v `CS.delete` smallestKnownCS)
         <> preferGoal v
         <> fromMaybe mempty (goalOrder sc)

    -- Like 'retry', except that it only applies the input function when the
    -- backjump limit has not been reached.
    retryNoSolution :: RetryLog step SolverFailure done
                    -> (ConflictSet -> ConflictMap -> RetryLog step SolverFailure done)
                    -> RetryLog step SolverFailure done
    retryNoSolution lg f = retry lg $ \case
        ExhaustiveSearch cs' cm' -> f cs' cm'
        BackjumpLimitReached     -> fromProgress (Fail BackjumpLimitReached)

-- | Goal ordering that chooses goals contained in the conflict set before
-- other goals.
preferGoalsFromConflictSet :: ConflictSet
                           -> Variable QPN -> Variable QPN -> Ordering
preferGoalsFromConflictSet cs = comparing $ \v -> not $ CS.member (toVar v) cs

-- | Goal ordering that chooses the given goal first.
preferGoal :: Var QPN -> Variable QPN -> Variable QPN -> Ordering
preferGoal preferred = comparing $ \v -> toVar v /= preferred

toVar :: Variable QPN -> Var QPN
toVar (PackageVar qpn)    = P qpn
toVar (FlagVar    qpn fn) = F (FN qpn fn)
toVar (StanzaVar  qpn sn) = S (SN qpn sn)

finalErrorMsg :: SolverConfig -> SolverFailure -> String
finalErrorMsg sc failure =
    case failure of
      ExhaustiveSearch cs cm ->
          "After searching the rest of the dependency tree exhaustively, "
          ++ "these were the goals I've had most trouble fulfilling: "
          ++ showCS cm cs
          ++ flagSuggestion
        where
          showCS = if solverVerbosity sc > Normal
                   then CS.showCSWithFrequency
                   else CS.showCSSortedByFrequency
          flagSuggestion =
              -- Don't suggest --minimize-conflict-set if the conflict set is
              -- already small, because it is unlikely to be reduced further.
              if CS.size cs > 3 && not (asBool (minimizeConflictSet sc))
              then "\nTry running with --minimize-conflict-set to improve the "
                    ++ "error message."
              else ""
      BackjumpLimitReached ->
          "Backjump limit reached (" ++ currlimit (maxBackjumps sc) ++
          "change with --max-backjumps or try to run with --reorder-goals).\n"
        where currlimit (Just n) = "currently " ++ show n ++ ", "
              currlimit Nothing  = ""
