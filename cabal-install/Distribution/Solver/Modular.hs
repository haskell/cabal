module Distribution.Solver.Modular
         ( modularResolver, SolverConfig(..), PruneAfterFirstSuccess(..)) where

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
import Data.Set (Set)
import Data.Ord
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
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.IndexConversion
         ( convPIs )
import Distribution.Solver.Modular.Log
         ( SolverFailure(..), logToProgress )
import Distribution.Solver.Modular.Package
         ( PN )
import Distribution.Solver.Modular.Solver
         ( SolverConfig(..), PruneAfterFirstSuccess(..), solve )
import Distribution.Solver.Types.DependencyResolver
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PackagePreferences
import Distribution.Solver.Types.PkgConfigDb
         ( PkgConfigDb )
import Distribution.Solver.Types.Progress
import Distribution.Solver.Types.Variable
import Distribution.System
         ( Platform(..) )
import Distribution.Simple.Utils
         ( ordNubBy )


-- | Ties the two worlds together: classic cabal-install vs. the modular
-- solver. Performs the necessary translations before and after.
modularResolver :: SolverConfig -> DependencyResolver loc
modularResolver sc (Platform arch os) cinfo iidx sidx pkgConfigDB pprefs pcs pns =
  fmap (uncurry postprocess) $ -- convert install plan
  solve' sc cinfo idx pkgConfigDB pprefs gcs pns
    where
      -- Indices have to be converted into solver-specific uniform index.
      idx    = convPIs os arch cinfo (shadowPkgs sc) (strongFlags sc) (solveExecutables sc) iidx sidx
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
-- first run. We also set the backjump limit to 0, so that the log stops at the
-- first backjump and is relatively short. Preferring goals from the final
-- conflict set increases the probability that the log to the first backjump
-- contains package, flag, and stanza choices that are relevant to the final
-- failure. The solver shouldn't need to choose any packages that aren't in the
-- final conflict set. (For every variable in the final conflict set, the final
-- conflict set should also contain the variable that introduced that variable.
-- The solver can then follow that chain of variables in reverse order from the
-- user target to the conflict.) However, it is possible that the conflict set
-- contains unnecessary variables.
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
       -> PkgConfigDb
       -> (PN -> PackagePreferences)
       -> Map PN [LabeledPackageConstraint]
       -> Set PN
       -> Progress String String (Assignment, RevDepMap)
solve' sc cinfo idx pkgConfigDB pprefs gcs pns =
    foldProgress Step createErrorMsg Done (runSolver sc)
  where
    runSolver :: SolverConfig
              -> Progress String SolverFailure (Assignment, RevDepMap)
    runSolver sc' =
        logToProgress (solverVerbosity sc') (maxBackjumps sc') $ -- convert log format into progress format
        solve sc' cinfo idx pkgConfigDB pprefs gcs pns

    createErrorMsg :: SolverFailure
                   -> Progress String String (Assignment, RevDepMap)
    createErrorMsg (NoSolution cs        msg) =
        Fail $ rerunSolverForErrorMsg cs ++ msg
    createErrorMsg (BackjumpLimitReached msg) =
        Step ("Backjump limit reached. Rerunning dependency solver to generate "
              ++ "a final conflict set for the search tree containing the "
              ++ "first backjump.") $
        foldProgress Step f Done $
        runSolver sc { pruneAfterFirstSuccess = PruneAfterFirstSuccess True }
      where
        f :: SolverFailure -> Progress String String (Assignment, RevDepMap)
        f (NoSolution cs        _) = Fail $ rerunSolverForErrorMsg cs ++ msg
        f (BackjumpLimitReached _) =
            -- This case is possible when the number of goals involved in
            -- conflicts is greater than the backjump limit.
            Fail $ msg ++ "Failed to generate a summarized dependency solver "
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

      in unlines ("Could not resolve dependencies:" : messages (runSolver sc'))

    messages :: Progress step fail done -> [step]
    messages = foldProgress (:) (const []) (const [])

-- | Goal ordering that chooses goals contained in the conflict set before
-- other goals.
preferGoalsFromConflictSet :: ConflictSet
                           -> Variable QPN -> Variable QPN -> Ordering
preferGoalsFromConflictSet cs =
    comparing $ \v -> not $ CS.member (toVar v) cs
  where
    toVar :: Variable QPN -> Var QPN
    toVar (PackageVar qpn)    = P qpn
    toVar (FlagVar    qpn fn) = F (FN qpn fn)
    toVar (StanzaVar  qpn sn) = S (SN qpn sn)
