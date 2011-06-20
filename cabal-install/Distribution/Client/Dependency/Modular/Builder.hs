module Distribution.Client.Dependency.Modular.Builder where

-- Building the search tree.

import Control.Monad.Reader hiding (sequence, mapM)
import Data.List as L
import Data.Map as M
import Data.Set as S
import Prelude hiding (sequence, mapM)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree

-- | The state needed during the build phase of the search tree.
data BuildState = BS {
  index :: Index,          -- ^ information about packages and their dependencies
  scope :: Scope,          -- ^ information about encapsulations
  goals :: RevDepMap,      -- ^ set of all package goals, completed and open, with reverse dependencies
  open  :: PSQ Goal (),    -- ^ set of still open goals
  next  :: BuildType       -- ^ kind of node to generate next
}

-- | Extend the set of open goals with the new goals listed.
--
-- We also adjust the map of overall goals, and keep track of the
-- reverse dependencies of each of the goals.
extendOpen :: QPN -> [Goal] -> BuildState -> BuildState
extendOpen qpn' gs s@(BS { goals = gs', open = o' }) = go gs' o' gs
  where
    go g o []                                         = s { goals = g, open = o }
    go g o (ng@(Goal (Flagged _ _ _ _)    _gr) : ngs) = go g (cons ng () o) ngs
    go g o (ng@(Goal (Simple (Dep qpn _)) _gr) : ngs)
      | qpn == qpn'                                   = go                       g              o  ngs
                                       -- we ignore self-dependencies at this point; TODO: more care may be needed
      | qpn `M.member` g                              = go (M.adjust (qpn':) qpn g)             o  ngs
      | otherwise                                     = go (M.insert qpn [qpn']  g) (cons ng () o) ngs
                                       -- code above is correct; insert/adjust have different arg order

-- | Update the current scope by taking into account the encapsulations that
-- are defined for the current package.
establishScope :: QPN -> Encaps -> BuildState -> BuildState
establishScope (Q pp pn) ecs s =
    s { scope = L.foldl (\ m e -> M.insert e pp' m) (scope s) ecs }
  where
    pp' = pn : pp -- new path

-- | Given the current scope, qualify all the package names in the given set of
-- dependencies and then extend the set of open goals accordingly.
scopedExtendOpen :: QPN -> I -> GoalReason -> FlaggedDeps PN -> FlagDefaults ->
                    BuildState -> BuildState
scopedExtendOpen qpn i gr fdeps fdefs s = extendOpen qpn gs s
  where
    sc     = scope s
    qfdeps = L.map (fmap (qualify sc)) fdeps -- qualify all the package names
    qfdefs = L.map (\ (fn, b) -> Flagged (FN (PI qpn i) fn) b [] []) $ M.toList fdefs
    gs     = L.map (flip Goal gr) (qfdeps ++ qfdefs)

data BuildType = Goals | OneGoal Goal | Instance QPN I PInfo

build :: BuildState -> Tree (GoalReason, Scope)
build = ana go
  where
    go :: BuildState -> TreeF (GoalReason, Scope) BuildState

    -- If we have a choice between many goals, we just record the choice in
    -- the tree. We select each open goal in turn, and before we descend, remove
    -- it from the queue of open goals.
    go bs@(BS { goals = rdeps, open = gs, next = Goals })
      | P.null gs = DoneF rdeps
      | otherwise = GoalChoiceF (P.mapWithKey (\ g (_sc, gs') -> bs { next = OneGoal g, open = gs' })
                                              (P.splits gs))

    -- If we have already picked a goal, then the choice depends on the kind
    -- of goal.
    --
    -- For a package, we look up the instances available in the global info,
    -- and then handle each instance in turn.
    go bs@(BS { index = idx, scope = sc, next = OneGoal (Goal (Simple (Dep qpn@(Q _ pn) _)) gr) }) =
      case M.lookup pn idx of
        Nothing  -> FailF (P qpn `S.insert` goalReasonToVars gr) (BuildFailureNotInIndex pn)
        Just pis -> PChoiceF qpn (gr, sc) (P.fromList (L.map (\ (i, info) ->
                                                           (i, bs { next = Instance qpn i  info }))
                                                         (M.toList pis)))
          -- TODO: data structure conversion is rather ugly here

    -- For a flag, we create only two subtrees, and we create them in the order
    -- that is indicated by the flag default.
    --
    -- TODO: Should we include the flag default in the tree?
    go bs@(BS { scope = sc, next = OneGoal (Goal (Flagged qfn b t f) gr) }) =
      FChoiceF qfn (gr, sc) trivial (P.fromList (reorder b
        [(True,  (extendOpen (getPN qfn) (L.map (flip Goal (FDependency qfn b)) t) bs) { next = Goals }),
         (False, (extendOpen (getPN qfn) (L.map (flip Goal (FDependency qfn b)) f) bs) { next = Goals })]))
      where
        reorder True  = id
        reorder False = reverse
        trivial = L.null t && L.null f

    -- For a particular instance, we change the state: we update the scope,
    -- and furthermore we update the set of goals.
    --
    -- TODO: We could inline this above.
    go bs@(BS { next = Instance qpn i (PInfo fdeps fdefs ecs) }) =
      go ((establishScope qpn ecs
             (scopedExtendOpen qpn i (PDependency (PI qpn i)) fdeps fdefs bs))
             { next = Goals })

-- | Interface to the tree builder. Just takes an index and a list of package names,
-- and computes the initial state and then the tree from there.
buildTree :: Index -> [PN] -> Tree (GoalReason, Scope)
buildTree idx igs =
    build (BS idx emptyScope
                  (M.fromList (L.map (\ qpn -> (qpn, []))                                               qpns))
                  (P.fromList (L.map (\ qpn -> (Goal (Simple (Dep qpn (Constrained []))) UserGoal, ())) qpns))
                  Goals)
  where
    qpns = L.map (qualify emptyScope) igs
