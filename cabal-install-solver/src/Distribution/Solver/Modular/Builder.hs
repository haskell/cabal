{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Solver.Modular.Builder (
    buildTree
  , splits -- for testing
  ) where

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

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude

import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.PSQ as P
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.WeightedPSQ as W

import Distribution.Solver.Types.ComponentDeps
import Distribution.Solver.Types.PackagePath
import qualified Distribution.Solver.Types.Stage as Stage

-- | All state needed to build and link the search tree. It has a type variable
-- because the linking phase doesn't need to know about the state used to build
-- the tree.
data Linker a = Linker {
  buildState   :: a,
  linkingState :: LinkingState
}

-- | The state needed to build the search tree without creating any linked nodes.
data BuildState = BS {
  index :: Index,                   -- ^ information about packages and their dependencies
  rdeps :: RevDepMap,               -- ^ set of all package goals, completed and open, with reverse dependencies
  open  :: [OpenGoal],              -- ^ set of still open goals (flag and package goals)
  next  :: BuildType                -- ^ kind of node to generate next
}

-- | Map of available linking targets.
type LinkingState = M.Map (PN, I) [PackagePath]

-- | Extend the set of open goals with the new goals listed.
--
-- We also adjust the map of overall goals, and keep track of the
-- reverse dependencies of each of the goals.
extendOpen :: QPN -> [FlaggedDep QPN] -> BuildState -> BuildState
extendOpen qpn deps buildState@(BS { rdeps = rdeps0, open = goals0 }) = go rdeps0 goals0 deps
  where
    go :: RevDepMap -> [OpenGoal] -> [FlaggedDep QPN] -> BuildState
    go rdeps goals [] =
      buildState { rdeps = rdeps, open = goals }

    go rdeps goals ((Flagged fn@(FN qpn' _) fInfo t f) : fdeps) =
      go rdeps (FlagGoal fn fInfo t f (flagGR qpn') : goals) fdeps

    -- Note: for 'Flagged' goals, we always insert, so later additions win.
    -- This is important, because in general, if a goal is inserted twice,
    -- the later addition will have better dependency information.
    go rdeps goals ((Stanza sn@(SN qpn' _) t) : fdeps) =
        go rdeps (StanzaGoal sn t (flagGR qpn') : goals) fdeps

    go rdeps goals ((Simple (LDep dr (Dep (PkgComponent qpn' _) _)) c) : fdeps)
      | qpn' == qpn =
          -- We currently only add a self-dependency to the graph if it is
          -- between a package and its setup script. The edge creates a cycle
          -- and causes the solver to backtrack and choose a different
          -- instance for the setup script. We may need to track other
          -- self-dependencies once we implement component-based solving.
          case c of
            ComponentSetup -> go (M.adjust (addIfAbsent (ComponentSetup, qpn)) qpn' rdeps) goals fdeps
            _              -> go rdeps  goals fdeps
      | qpn' `M.member` rdeps =
          go (M.adjust (addIfAbsent (c, qpn)) qpn' rdeps) goals fdeps
      | otherwise =
          -- Note: insert/adjust have different arg order
          go (M.insert qpn' [(c, qpn)] rdeps) (PkgGoal qpn' (DependencyGoal dr) : goals) fdeps

    go rdeps o ((Simple (LDep _dr (Ext _ext )) _c) : goals) = go rdeps o goals
    go rdeps o ((Simple (LDep _dr (Lang _lang)) _c) : goals) = go rdeps o goals
    go rdeps o ((Simple (LDep _dr (Pkg _pn _vr)) _c) : goals) = go rdeps o goals

    addIfAbsent :: Eq a => a -> [a] -> [a]
    addIfAbsent x xs = if x `elem` xs then xs else x : xs

-- GoalReason for a flag or stanza. Each flag/stanza is introduced only by
-- its containing package.
flagGR :: qpn -> GoalReason qpn
flagGR qpn = DependencyGoal (DependencyReason qpn M.empty S.empty)

-- | Given the current scope, qualify all the package names in the given set of
-- dependencies and then extend the set of open goals accordingly.
scopedExtendOpen :: QPN -> FlaggedDeps PN -> FlagInfo ->
                    BuildState -> BuildState
scopedExtendOpen qpn fdeps fdefs s = extendOpen qpn gs s
  where
    -- Qualify all package names
    qfdeps = qualifyDeps qpn fdeps
    -- Introduce all package flags
    qfdefs = L.map (\ (fn, b) -> Flagged (FN qpn fn) b [] []) $ M.toList fdefs
    -- Combine new package and flag goals
    gs     = qfdefs ++ qfdeps
    -- NOTE:
    --
    -- In the expression @qfdefs ++ qfdeps@ above, flags occur potentially
    -- multiple times, both via the flag declaration and via dependencies.

-- | Datatype that encodes what to build next
data BuildType =
    Goals              -- ^ build a goal choice node
  | OneGoal OpenGoal   -- ^ build a node for this goal
  | Instance QPN PInfo -- ^ build a tree for a concrete instance

build :: Linker BuildState -> Tree () QGoalReason
build = ana go
  where
    go :: Linker BuildState -> TreeF () QGoalReason (Linker BuildState)
    go s = addLinking (linkingState s) $ addChildren (buildState s)

-- | Add children to the tree based on the current build state.
addChildren :: BuildState -> TreeF () QGoalReason BuildState

-- If we have a choice between many goals, we just record the choice in
-- the tree. We select each open goal in turn, and before we descend, remove
-- it from the queue of open goals.
addChildren bs@(BS { rdeps = rdm, open = gs, next = Goals })
  -- No goals left. We have done.
  | L.null gs = DoneF rdm ()
  | otherwise = GoalChoiceF rdm $ P.fromList
                                $ L.map (\ (g, gs') -> (close g, bs { next = OneGoal g, open = gs' }))
                                $ splits gs

-- If we have already picked a goal, then the choice depends on the kind
-- of goal.
addChildren bs@(BS { rdeps, index, next = OneGoal goal }) = 
  case goal of 
    PkgGoal qpn@(Q (PackagePath s _) pn) gr ->
      -- For a package goal, we look up the instances available in the global
      -- info, and then handle each instance in turn.
      case M.lookup pn index of
        Nothing  -> FailF
                    (varToConflictSet (P qpn) `CS.union` goalReasonToConflictSetWithConflict qpn gr)
                    UnknownPackage
        Just pis -> PChoiceF qpn rdeps gr $ W.fromList
                      [ ([], POption i Nothing, bs { next = Instance qpn info })
                      | (i@(I s' _ver _loc), info) <- M.toList pis
                      -- Only instances belonging to the same stage are allowed.
                      , s == s'
                      ]
    -- For a flag, we create only two subtrees, and we create them in the order
    -- that is indicated by the flag default.
    FlagGoal qfn@(FN qpn _) (FInfo b m w) t f gr ->
        FChoiceF qfn rdeps gr weak m b $ W.fromList
          [ ([if b then 0 else 1], True,  (extendOpen qpn t bs) { next = Goals })
          , ([if b then 1 else 0], False, (extendOpen qpn f bs) { next = Goals })
          ]
        where
          trivial = L.null t && L.null f
          weak = WeakOrTrivial $ unWeakOrTrivial w || trivial
    -- For a stanza, we also create only two subtrees. The order is initially
    -- False, True. This can be changed later by constraints (force enabling
    -- the stanza by replacing the False branch with failure) or preferences
    -- (try enabling the stanza if possible by moving the True branch first).
    StanzaGoal qsn@(SN qpn _) t gr ->
      SChoiceF qsn rdeps gr trivial $ W.fromList
        [ ([0], False, bs { next = Goals })
        , ([1], True,  (extendOpen qpn t bs) { next = Goals })
        ]
      where
        trivial = WeakOrTrivial (L.null t)

-- For a particular instance, we change the state: we update the scope,
-- and furthermore we update the set of goals.
--
-- TODO: We could inline this above.
addChildren bs@(BS { next = Instance qpn (PInfo fdeps _ fdefs _) }) =
  addChildren ((scopedExtendOpen qpn fdeps fdefs bs)
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
buildTree :: Index -> [PN] -> Tree () QGoalReason
buildTree idx igs =
    build Linker {
        buildState = BS {
            index = idx
          , rdeps = M.fromList [(qpn, []) | qpn <- qpns]
          , open  = [ PkgGoal qpn UserGoal | qpn <- qpns ]
          , next  = Goals
          }
      , linkingState = M.empty
      }
  where
    -- The package names are interpreted as top-level goals in the host stage.
    path = PackagePath Stage.Host QualToplevel
    qpns = [ Q path pn | pn <- igs ]


{-------------------------------------------------------------------------------
  Goals
-------------------------------------------------------------------------------}

-- | Information needed about a dependency before it is converted into a Goal.
data OpenGoal =
    FlagGoal   (FN QPN) FInfo (FlaggedDeps QPN) (FlaggedDeps QPN) QGoalReason
  | StanzaGoal (SN QPN)       (FlaggedDeps QPN)                   QGoalReason
  | PkgGoal    QPN                                                QGoalReason

-- | Closes a goal, i.e., removes all the extraneous information that we
-- need only during the build phase.
close :: OpenGoal -> Goal QPN
close (FlagGoal   qfn _ _ _ gr) = Goal (F qfn) gr
close (StanzaGoal qsn _     gr) = Goal (S qsn) gr
close (PkgGoal    qpn       gr) = Goal (P qpn) gr

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Pairs each element of a list with the list resulting from removal of that
-- element from the original list.
splits :: [a] -> [(a, [a])]
splits = go id
  where
    go :: ([a] -> [a]) -> [a] -> [(a, [a])]
    go _ [] = []
    go f (x : xs) = (x, f xs) : go (f . (x :)) xs
