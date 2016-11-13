{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Solver.Modular.Builder (buildTree, addLinking) where

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

import Control.Monad.Reader
import Data.List as L
import Data.Map as M
import Prelude hiding (sequence, mapM)

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.PSQ (PSQ)
import qualified Distribution.Solver.Modular.PSQ as P
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.WeightedPSQ as W

import Distribution.Solver.Types.ComponentDeps (Component)
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Settings

type Linker       = Reader RelatedGoals

-- | The state needed during the build phase of the search tree.
data BuildState = BS {
  index :: Index,                -- ^ information about packages and their dependencies
  rdeps :: RevDepMap,            -- ^ set of all package goals, completed and open, with reverse dependencies
  open  :: PSQ (OpenGoal ()) (), -- ^ set of still open goals (flag and package goals)
  next  :: BuildType,            -- ^ kind of node to generate next
  qualifyOptions :: QualifyOptions -- ^ qualification options
}

type RelatedGoals = Map (PN, I) [PackagePath]

-- | Extend the set of open goals with the new goals listed.
--
-- We also adjust the map of overall goals, and keep track of the
-- reverse dependencies of each of the goals.
extendOpen :: QPN -> [OpenGoal Component] -> BuildState -> BuildState
extendOpen qpn' gs s@(BS { rdeps = gs', open = o' }) = go gs' o' gs
  where
    go :: RevDepMap -> PSQ (OpenGoal ()) () -> [OpenGoal Component] -> BuildState
    go g o []                                               = s { rdeps = g, open = o }
    go g o (ng@(OpenGoal (Flagged _ _ _ _)      _gr) : ngs) = go g (cons' ng () o) ngs
      -- Note: for 'Flagged' goals, we always insert, so later additions win.
      -- This is important, because in general, if a goal is inserted twice,
      -- the later addition will have better dependency information.
    go g o (ng@(OpenGoal (Stanza  _   _  )      _gr) : ngs) = go g (cons' ng () o) ngs
    go g o (ng@(OpenGoal (Simple (Dep _ qpn _) c) _gr) : ngs)
      | qpn == qpn'       = go                            g               o  ngs
          -- we ignore self-dependencies at this point; TODO: more care may be needed
      | qpn `M.member` g  = go (M.adjust ((c, qpn'):) qpn g)              o  ngs
      | otherwise         = go (M.insert qpn [(c, qpn')]  g) (cons' ng () o) ngs
          -- code above is correct; insert/adjust have different arg order
    go g o (   (OpenGoal (Simple (Ext _ext ) _) _gr) : ngs) = go g o ngs
    go g o (   (OpenGoal (Simple (Lang _lang)_) _gr) : ngs) = go g o ngs
    go g o (   (OpenGoal (Simple (Pkg _pn _vr)_) _gr) : ngs)= go g o ngs

    cons' = P.cons . forgetCompOpenGoal

-- | Given the current scope, qualify all the package names in the given set of
-- dependencies and then extend the set of open goals accordingly.
scopedExtendOpen :: QPN -> I -> QGoalReason -> FlaggedDeps Component PN -> FlagInfo ->
                    BuildState -> BuildState
scopedExtendOpen qpn i gr fdeps fdefs s = extendOpen qpn gs s
  where
    -- Qualify all package names
    qfdeps = qualifyDeps (qualifyOptions s) qpn fdeps
    -- Introduce all package flags
    qfdefs = L.map (\ (fn, b) -> Flagged (FN (PI qpn i) fn) b [] []) $ M.toList fdefs
    -- Combine new package and flag goals
    gs     = L.map (flip OpenGoal gr) (qfdefs ++ qfdeps)
    -- NOTE:
    --
    -- In the expression @qfdefs ++ qfdeps@ above, flags occur potentially
    -- multiple times, both via the flag declaration and via dependencies.
    -- The order is potentially important, because the occurrences via
    -- dependencies may record flag-dependency information. After a number
    -- of bugs involving computing this information incorrectly, however,
    -- we're currently not using carefully computed inter-flag dependencies
    -- anymore, but instead use 'simplifyVar' when computing conflict sets
    -- to map all flags of one package to a single flag for conflict set
    -- purposes, thereby treating them all as interdependent.
    --
    -- If we ever move to a more clever algorithm again, then the line above
    -- needs to be looked at very carefully, and probably be replaced by
    -- more systematically computed flag dependency information.

-- | Datatype that encodes what to build next
data BuildType =
    Goals                                  -- ^ build a goal choice node
  | OneGoal (OpenGoal ())                  -- ^ build a node for this goal
  | Instance QPN I PInfo QGoalReason  -- ^ build a tree for a concrete instance
  deriving Show

build :: BuildState -> Tree () QGoalReason
build = ana go
  where
    go :: BuildState -> TreeF () QGoalReason BuildState

    -- If we have a choice between many goals, we just record the choice in
    -- the tree. We select each open goal in turn, and before we descend, remove
    -- it from the queue of open goals.
    go bs@(BS { rdeps = rds, open = gs, next = Goals })
      | P.null gs = DoneF rds ()
      | otherwise = GoalChoiceF $ P.mapKeys close
                                $ P.mapWithKey (\ g (_sc, gs') -> bs { next = OneGoal g, open = gs' })
                                $ P.splits gs

    -- If we have already picked a goal, then the choice depends on the kind
    -- of goal.
    --
    -- For a package, we look up the instances available in the global info,
    -- and then handle each instance in turn.
    go    (BS { index = _  , next = OneGoal (OpenGoal (Simple (Ext _             ) _) _ ) }) =
      error "Distribution.Solver.Modular.Builder: build.go called with Ext goal"
    go    (BS { index = _  , next = OneGoal (OpenGoal (Simple (Lang _            ) _) _ ) }) =
      error "Distribution.Solver.Modular.Builder: build.go called with Lang goal"
    go    (BS { index = _  , next = OneGoal (OpenGoal (Simple (Pkg _ _          ) _) _ ) }) =
      error "Distribution.Solver.Modular.Builder: build.go called with Pkg goal"
    go bs@(BS { index = idx, next = OneGoal (OpenGoal (Simple (Dep _ qpn@(Q _ pn) _) _) gr) }) =
      -- If the package does not exist in the index, we construct an emty PChoiceF node for it
      -- After all, we have no choices here. Alternatively, we could immediately construct
      -- a Fail node here, but that would complicate the construction of conflict sets.
      -- We will probably want to give this case special treatment when generating error
      -- messages though.
      case M.lookup pn idx of
        Nothing  -> PChoiceF qpn gr (W.fromList [])
        Just pis -> PChoiceF qpn gr (W.fromList (L.map (\ (i, info) ->
                                                           ([], POption i Nothing, bs { next = Instance qpn i info gr }))
                                                         (M.toList pis)))
          -- TODO: data structure conversion is rather ugly here

    -- For a flag, we create only two subtrees, and we create them in the order
    -- that is indicated by the flag default.
    --
    -- TODO: Should we include the flag default in the tree?
    go bs@(BS { next = OneGoal (OpenGoal (Flagged qfn@(FN (PI qpn _) _) (FInfo b m w) t f) gr) }) =
      FChoiceF qfn gr weak m (W.fromList
        [([if b then 0 else 1], True,  (extendOpen qpn (L.map (flip OpenGoal (FDependency qfn True )) t) bs) { next = Goals }),
         ([if b then 1 else 0], False, (extendOpen qpn (L.map (flip OpenGoal (FDependency qfn False)) f) bs) { next = Goals })])
      where
        trivial = L.null t && L.null f
        weak = WeakOrTrivial $ unWeakOrTrivial w || trivial

    -- For a stanza, we also create only two subtrees. The order is initially
    -- False, True. This can be changed later by constraints (force enabling
    -- the stanza by replacing the False branch with failure) or preferences
    -- (try enabling the stanza if possible by moving the True branch first).

    go bs@(BS { next = OneGoal (OpenGoal (Stanza qsn@(SN (PI qpn _) _) t) gr) }) =
      SChoiceF qsn gr trivial (W.fromList
        [([0], False,                                                             bs  { next = Goals }),
         ([1], True,  (extendOpen qpn (L.map (flip OpenGoal (SDependency qsn)) t) bs) { next = Goals })])
      where
        trivial = WeakOrTrivial (L.null t)

    -- For a particular instance, we change the state: we update the scope,
    -- and furthermore we update the set of goals.
    --
    -- TODO: We could inline this above.
    go bs@(BS { next = Instance qpn i (PInfo fdeps fdefs _) _gr }) =
      go ((scopedExtendOpen qpn i (PDependency (PI qpn i)) fdeps fdefs bs)
             { next = Goals })

{-------------------------------------------------------------------------------
  Add linking
-------------------------------------------------------------------------------}

-- | Introduce link nodes into the tree
--
-- Linking is a traversal of the solver tree that adapts package choice nodes
-- and adds the option to link wherever appropriate: Package goals are called
-- "related" if they are for the same instance of the same package (but have
-- different prefixes). A link option is available in a package choice node
-- whenever we can choose an instance that has already been chosen for a related
-- goal at a higher position in the tree. We only create link options for
-- related goals that are not themselves linked, because the choice to link to a
-- linked goal is the same as the choice to link to the target of that goal's
-- linking.
--
-- The code here proceeds by maintaining a finite map recording choices that
-- have been made at higher positions in the tree. For each pair of package name
-- and instance, it stores the prefixes at which we have made a choice for this
-- package instance. Whenever we make an unlinked choice, we extend the map.
-- Whenever we find a choice, we look into the map in order to find out what
-- link options we have to add.
addLinking :: Tree d c -> Tree d c
addLinking = (`runReader` M.empty) .  cata go
  where
    go :: TreeF d c (Linker (Tree d c)) -> Linker (Tree d c)

    -- The only nodes of interest are package nodes
    go (PChoiceF qpn gr cs) = do
      env <- ask
      let linkedCs = W.fromList $ concatMap (linkChoices env qpn) (W.toList cs)
          unlinkedCs = W.mapWithKey (goP qpn) cs
      allCs <- sequence $ unlinkedCs `W.union` linkedCs
      return $ PChoice qpn gr allCs
    go _otherwise =
      innM _otherwise

    -- Recurse underneath package choices. Here we just need to make sure
    -- that we record the package choice so that it is available below
    goP :: QPN -> POption -> Linker (Tree d c) -> Linker (Tree d c)
    goP (Q pp pn) (POption i Nothing) = local (M.insertWith (++) (pn, i) [pp])
    goP _ _ = alreadyLinked

linkChoices :: forall a w . RelatedGoals
            -> QPN
            -> (w, POption, a)
            -> [(w, POption, a)]
linkChoices related (Q _pp pn) (weight, POption i Nothing, subtree) =
    L.map aux (M.findWithDefault [] (pn, i) related)
  where
    aux :: PackagePath -> (w, POption, a)
    aux pp = (weight, POption i (Just pp), subtree)
linkChoices _ _ (_, POption _ (Just _), _) =
    alreadyLinked

alreadyLinked :: a
alreadyLinked = error "addLinking called on tree that already contains linked nodes"

-------------------------------------------------------------------------------

-- | Interface to the tree builder. Just takes an index and a list of package names,
-- and computes the initial state and then the tree from there.
buildTree :: Index -> IndependentGoals -> [PN] -> Tree () QGoalReason
buildTree idx (IndependentGoals ind) igs =
    build BS {
        index = idx
      , rdeps = M.fromList (L.map (\ qpn -> (qpn, []))              qpns)
      , open  = P.fromList (L.map (\ qpn -> (topLevelGoal qpn, ())) qpns)
      , next  = Goals
      , qualifyOptions = defaultQualifyOptions idx
      }
  where
    -- Should a top-level goal allowed to be an executable style
    -- dependency? Well, I don't think it would make much difference
    topLevelGoal qpn = OpenGoal (Simple (Dep False {- not exe -} qpn (Constrained [])) ()) UserGoal

    qpns | ind       = makeIndependent igs
         | otherwise = L.map (Q (PackagePath DefaultNamespace Unqualified)) igs
