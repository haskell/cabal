{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Solver.Modular.Builder (buildTree) where

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

-- | All state needed to build and link the search tree. It has a type variable
-- because the linking phase doesn't need to know about the state used to build
-- the tree.
data Linker a = Linker {
  buildState   :: a,
  linkingState :: LinkingState
}

-- | The state needed to build the search tree without creating any linked nodes.
data BuildState = BS {
  index :: Index,                -- ^ information about packages and their dependencies
  rdeps :: RevDepMap,            -- ^ set of all package goals, completed and open, with reverse dependencies
  open  :: PSQ (OpenGoal ()) (), -- ^ set of still open goals (flag and package goals)
  next  :: BuildType,            -- ^ kind of node to generate next
  qualifyOptions :: QualifyOptions -- ^ qualification options
}

-- | Map of available linking targets.
type LinkingState = Map (PN, I) [PackagePath]

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
      | qpn `M.member` g  = go (M.adjust (addIfAbsent (c, qpn')) qpn g)   o  ngs
      | otherwise         = go (M.insert qpn [(c, qpn')]  g) (cons' ng () o) ngs
          -- code above is correct; insert/adjust have different arg order
    go g o (   (OpenGoal (Simple (Ext _ext ) _) _gr) : ngs) = go g o ngs
    go g o (   (OpenGoal (Simple (Lang _lang)_) _gr) : ngs) = go g o ngs
    go g o (   (OpenGoal (Simple (Pkg _pn _vr)_) _gr) : ngs)= go g o ngs

    cons' = P.cons . forgetCompOpenGoal

    addIfAbsent :: Eq a => a -> [a] -> [a]
    addIfAbsent x xs = if x `elem` xs then xs else x : xs

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

build :: Linker BuildState -> Tree () QGoalReason
build = ana go
  where
    go :: Linker BuildState -> TreeF () QGoalReason (Linker BuildState)
    go s = addLinking (linkingState s) $ addChildren (buildState s)

addChildren :: BuildState -> TreeF () QGoalReason BuildState

-- If we have a choice between many goals, we just record the choice in
-- the tree. We select each open goal in turn, and before we descend, remove
-- it from the queue of open goals.
addChildren bs@(BS { rdeps = rdm, open = gs, next = Goals })
  | P.null gs = DoneF rdm ()
  | otherwise = GoalChoiceF rdm $ P.mapKeys close
                                $ P.mapWithKey (\ g (_sc, gs') -> bs { next = OneGoal g, open = gs' })
                                $ P.splits gs

-- If we have already picked a goal, then the choice depends on the kind
-- of goal.
--
-- For a package, we look up the instances available in the global info,
-- and then handle each instance in turn.
addChildren    (BS { index = _  , next = OneGoal (OpenGoal (Simple (Ext _             ) _) _ ) }) =
  error "Distribution.Solver.Modular.Builder: addChildren called with Ext goal"
addChildren    (BS { index = _  , next = OneGoal (OpenGoal (Simple (Lang _            ) _) _ ) }) =
  error "Distribution.Solver.Modular.Builder: addChildren called with Lang goal"
addChildren    (BS { index = _  , next = OneGoal (OpenGoal (Simple (Pkg _ _          ) _) _ ) }) =
  error "Distribution.Solver.Modular.Builder: addChildren called with Pkg goal"
addChildren bs@(BS { rdeps = rdm, index = idx, next = OneGoal (OpenGoal (Simple (Dep _ qpn@(Q _ pn) _) _) gr) }) =
  -- If the package does not exist in the index, we construct an emty PChoiceF node for it
  -- After all, we have no choices here. Alternatively, we could immediately construct
  -- a Fail node here, but that would complicate the construction of conflict sets.
  -- We will probably want to give this case special treatment when generating error
  -- messages though.
  case M.lookup pn idx of
    Nothing  -> PChoiceF qpn rdm gr (W.fromList [])
    Just pis -> PChoiceF qpn rdm gr (W.fromList (L.map (\ (i, info) ->
                                                       ([], POption i Nothing, bs { next = Instance qpn i info gr }))
                                                     (M.toList pis)))
      -- TODO: data structure conversion is rather ugly here

-- For a flag, we create only two subtrees, and we create them in the order
-- that is indicated by the flag default.
addChildren bs@(BS { rdeps = rdm, next = OneGoal (OpenGoal (Flagged qfn@(FN (PI qpn _) _) (FInfo b m w) t f) gr) }) =
  FChoiceF qfn rdm gr weak m b (W.fromList
    [([if b then 0 else 1], True,  (extendOpen qpn (L.map (flip OpenGoal (FDependency qfn True )) t) bs) { next = Goals }),
     ([if b then 1 else 0], False, (extendOpen qpn (L.map (flip OpenGoal (FDependency qfn False)) f) bs) { next = Goals })])
  where
    trivial = L.null t && L.null f
    weak = WeakOrTrivial $ unWeakOrTrivial w || trivial

-- For a stanza, we also create only two subtrees. The order is initially
-- False, True. This can be changed later by constraints (force enabling
-- the stanza by replacing the False branch with failure) or preferences
-- (try enabling the stanza if possible by moving the True branch first).

addChildren bs@(BS { rdeps = rdm, next = OneGoal (OpenGoal (Stanza qsn@(SN (PI qpn _) _) t) gr) }) =
  SChoiceF qsn rdm gr trivial (W.fromList
    [([0], False,                                                             bs  { next = Goals }),
     ([1], True,  (extendOpen qpn (L.map (flip OpenGoal (SDependency qsn)) t) bs) { next = Goals })])
  where
    trivial = WeakOrTrivial (L.null t)

-- For a particular instance, we change the state: we update the scope,
-- and furthermore we update the set of goals.
--
-- TODO: We could inline this above.
addChildren bs@(BS { next = Instance qpn i (PInfo fdeps fdefs _) _gr }) =
  addChildren ((scopedExtendOpen qpn i (PDependency (PI qpn i)) fdeps fdefs bs)
         { next = Goals })

{-------------------------------------------------------------------------------
  Add linking
-------------------------------------------------------------------------------}

-- | Introduce link nodes into the tree
--
-- Linking is a phase that adapts package choice nodes and adds the option to
-- link wherever appropriate: Package goals are called "related" if they are for
-- the same instance of the same package (but have different prefixes). A link
-- option is available in a package choice node whenever we can choose an
-- instance that has already been chosen for a related goal at a higher position
-- in the tree. We only create link options for related goals that are not
-- themselves linked, because the choice to link to a linked goal is the same as
-- the choice to link to the target of that goal's linking.
--
-- The code here proceeds by maintaining a finite map recording choices that
-- have been made at higher positions in the tree. For each pair of package name
-- and instance, it stores the prefixes at which we have made a choice for this
-- package instance. Whenever we make an unlinked choice, we extend the map.
-- Whenever we find a choice, we look into the map in order to find out what
-- link options we have to add.
--
-- A separate tree traversal would be simpler. However, 'addLinking' creates
-- linked nodes from existing unlinked nodes, which leads to sharing between the
-- nodes. If we copied the nodes when they were full trees of type
-- 'Tree () QGoalReason', then the sharing would cause a space leak during
-- exploration of the tree. Instead, we only copy the 'BuildState', which is
-- relatively small, while the tree is being constructed. See
-- https://github.com/haskell/cabal/issues/2899
addLinking :: LinkingState -> TreeF () c a -> TreeF () c (Linker a)
-- The only nodes of interest are package nodes
addLinking ls (PChoiceF qpn@(Q pp pn) rdm gr cs) =
  let linkedCs = fmap (\bs -> Linker bs ls) $
                 W.fromList $ concatMap (linkChoices ls qpn) (W.toList cs)
      unlinkedCs = W.mapWithKey goP cs
      allCs = unlinkedCs `W.union` linkedCs

      -- Recurse underneath package choices. Here we just need to make sure
      -- that we record the package choice so that it is available below
      goP :: POption -> a -> Linker a
      goP (POption i Nothing) bs = Linker bs $ M.insertWith (++) (pn, i) [pp] ls
      goP _                   _  = alreadyLinked
  in PChoiceF qpn rdm gr allCs
addLinking ls t = fmap (\bs -> Linker bs ls) t

linkChoices :: forall a w . LinkingState
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
    build Linker {
        buildState = BS {
            index = idx
          , rdeps = M.fromList (L.map (\ qpn -> (qpn, []))              qpns)
          , open  = P.fromList (L.map (\ qpn -> (topLevelGoal qpn, ())) qpns)
          , next  = Goals
          , qualifyOptions = defaultQualifyOptions idx
          }
      , linkingState = M.empty
      }
  where
    -- Should a top-level goal allowed to be an executable style
    -- dependency? Well, I don't think it would make much difference
    topLevelGoal qpn = OpenGoal (Simple (Dep False {- not exe -} qpn (Constrained [])) ()) UserGoal

    qpns | ind       = makeIndependent igs
         | otherwise = L.map (Q (PackagePath DefaultNamespace QualToplevel)) igs
