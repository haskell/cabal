module Distribution.Client.Dependency.Modular.Builder where

-- Building the search tree.
--
-- In this phase, we build a search tree that is too large, i.e, it contains
-- invalid solutions. We keep track of the open goals at each point. We
-- nondeterministically pick an open goal (via a goal choice node), create
-- subtrees according to the index and the available solutions, and extend the
-- set of open goals by superficially looking at the dependencies recorded in
-- the index.
--
-- For each goal, we keep track of all the *reasons* why it is being
-- introduced. These are for debugging and error messages, mainly. A little bit
-- of care has to be taken due to the way we treat flags. If a package has
-- flag-guarded dependencies, we cannot introduce them immediately. Instead, we
-- store the entire dependency.

import Control.Monad.Reader hiding (sequence, mapM)
import Data.List as L
import Data.Map as M
import Prelude hiding (sequence, mapM)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree

-- | The state needed during the build phase of the search tree.
data BuildState = BS {
  index :: Index,           -- ^ information about packages and their dependencies
  scope :: Scope,           -- ^ information about encapsulations
  rdeps :: RevDepMap,       -- ^ set of all package goals, completed and open, with reverse dependencies
  open  :: PSQ OpenGoal (), -- ^ set of still open goals (flag and package goals)
  next  :: BuildType        -- ^ kind of node to generate next
}

-- | Extend the set of open goals with the new goals listed.
--
-- We also adjust the map of overall goals, and keep track of the
-- reverse dependencies of each of the goals.
extendOpen :: QPN -> [OpenGoal] -> BuildState -> BuildState
extendOpen qpn' gs s@(BS { rdeps = gs', open = o' }) = go gs' o' gs
  where
    go :: RevDepMap -> PSQ OpenGoal () -> [OpenGoal] -> BuildState
    go g o []                                             = s { rdeps = g, open = o }
    go g o (ng@(OpenGoal (Flagged _ _ _ _)    _gr) : ngs) = go g (cons ng () o) ngs
    go g o (ng@(OpenGoal (Stanza  _   _  )    _gr) : ngs) = go g (cons ng () o) ngs
    go g o (ng@(OpenGoal (Simple (Dep qpn _)) _gr) : ngs)
      | qpn == qpn'                                       = go                       g              o  ngs
                                       -- we ignore self-dependencies at this point; TODO: more care may be needed
      | qpn `M.member` g                                  = go (M.adjust (qpn':) qpn g)             o  ngs
      | otherwise                                         = go (M.insert qpn [qpn']  g) (cons ng () o) ngs
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
scopedExtendOpen :: QPN -> I -> QGoalReasonChain -> FlaggedDeps PN -> FlagInfo ->
                    BuildState -> BuildState
scopedExtendOpen qpn i gr fdeps fdefs s = extendOpen qpn gs s
  where
    sc     = scope s
    qfdeps = L.map (fmap (qualify sc)) fdeps -- qualify all the package names
    qfdefs = L.map (\ (fn, b) -> Flagged (FN (PI qpn i) fn) b [] []) $ M.toList fdefs
    gs     = L.map (flip OpenGoal gr) (qfdeps ++ qfdefs)

data BuildType = Goals | OneGoal OpenGoal | Instance QPN I PInfo QGoalReasonChain

build :: BuildState -> Tree (QGoalReasonChain, Scope)
build = ana go
  where
    go :: BuildState -> TreeF (QGoalReasonChain, Scope) BuildState

    -- If we have a choice between many goals, we just record the choice in
    -- the tree. We select each open goal in turn, and before we descend, remove
    -- it from the queue of open goals.
    go bs@(BS { rdeps = rds, open = gs, next = Goals })
      | P.null gs = DoneF rds
      | otherwise = GoalChoiceF (P.mapWithKey (\ g (_sc, gs') -> bs { next = OneGoal g, open = gs' })
                                              (P.splits gs))

    -- If we have already picked a goal, then the choice depends on the kind
    -- of goal.
    --
    -- For a package, we look up the instances available in the global info,
    -- and then handle each instance in turn.
    go bs@(BS { index = idx, scope = sc, next = OneGoal (OpenGoal (Simple (Dep qpn@(Q _ pn) _)) gr) }) =
      case M.lookup pn idx of
        Nothing  -> FailF (toConflictSet (Goal (P qpn) gr)) (BuildFailureNotInIndex pn)
        Just pis -> PChoiceF qpn (gr, sc) (P.fromList (L.map (\ (i, info) ->
                                                           (i, bs { next = Instance qpn i info gr }))
                                                         (M.toList pis)))
          -- TODO: data structure conversion is rather ugly here

    -- For a flag, we create only two subtrees, and we create them in the order
    -- that is indicated by the flag default.
    --
    -- TODO: Should we include the flag default in the tree?
    go bs@(BS { scope = sc, next = OneGoal (OpenGoal (Flagged qfn@(FN (PI qpn _) _) (FInfo b m) t f) gr) }) =
      FChoiceF qfn (gr, sc) trivial m (P.fromList (reorder b
        [(True,  (extendOpen qpn (L.map (flip OpenGoal (FDependency qfn True  : gr)) t) bs) { next = Goals }),
         (False, (extendOpen qpn (L.map (flip OpenGoal (FDependency qfn False : gr)) f) bs) { next = Goals })]))
      where
        reorder True  = id
        reorder False = reverse
        trivial = L.null t && L.null f

    go bs@(BS { scope = sc, next = OneGoal (OpenGoal (Stanza qsn@(SN (PI qpn _) _) t) gr) }) =
      SChoiceF qsn (gr, sc) trivial (P.fromList
        [(False,                                                                        bs  { next = Goals }),
         (True,  (extendOpen qpn (L.map (flip OpenGoal (SDependency qsn : gr)) t) bs) { next = Goals })])
      where
        trivial = L.null t

    -- For a particular instance, we change the state: we update the scope,
    -- and furthermore we update the set of goals.
    --
    -- TODO: We could inline this above.
    go bs@(BS { next = Instance qpn i (PInfo fdeps fdefs ecs _) gr }) =
      go ((establishScope qpn ecs
             (scopedExtendOpen qpn i (PDependency (PI qpn i) : gr) fdeps fdefs bs))
             { next = Goals })

-- | Interface to the tree builder. Just takes an index and a list of package names,
-- and computes the initial state and then the tree from there.
buildTree :: Index -> Bool -> [PN] -> Tree (QGoalReasonChain, Scope)
buildTree idx ind igs =
    build (BS idx sc
                  (M.fromList (L.map (\ qpn -> (qpn, []))                                                     qpns))
                  (P.fromList (L.map (\ qpn -> (OpenGoal (Simple (Dep qpn (Constrained []))) [UserGoal], ())) qpns))
                  Goals)
  where
    sc | ind       = makeIndependent igs
       | otherwise = emptyScope
    qpns           = L.map (qualify sc) igs
