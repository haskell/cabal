module Distribution.Client.Dependency.Modular.Explore where

import Control.Applicative as A
import Data.Foldable
import Data.Map as M

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Log
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree

-- | Naive backtracking exploration of the search tree. This will yield correct
-- assignments only once the tree itself is validated.
explore :: Alternative m => Tree a -> (Assignment -> m (Assignment, RevDepMap))
explore = cata go
  where
    go (FailF _ _)           _           = A.empty
    go (DoneF rdm)           a           = pure (a, rdm)
    go (PChoiceF qpn _   ts) (A pa fa)   =
      asum $                                      -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ k r -> r (A (M.insert qpn k pa) fa)) $ -- record the pkg choice
      ts
    go (FChoiceF qfn _ _ ts) (A pa fa)   =
      asum $                                      -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ k r -> r (A pa (M.insert qfn k fa))) $ -- record the flag choice
      ts
    go (GoalChoiceF      ts) a           =
      casePSQ ts A.empty                      -- empty goal choice is an internal error
        (\ _k v _xs -> v a)                   -- commit to the first goal choice

-- | Version of 'explore' that returns a 'Log'.
exploreLog :: Tree a -> (Assignment -> Log Message (Assignment, RevDepMap))
exploreLog = cata go
  where
    go (FailF _ fr)          _           = failWith (Failure fr)
    go (DoneF rdm)           a           = succeedWith Success (a, rdm)
    go (PChoiceF qpn _   ts) (A pa fa)   =
      asum $                                      -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryP (PI qpn k)) $     -- log and ...
                    r (A (M.insert qpn k pa) fa)) -- record the pkg choice
      ts
    go (FChoiceF qfn _ _ ts) (A pa fa)   =
      asum $                                      -- try children in order,
      P.mapWithKey                                -- when descending ...
        (\ k r -> tryWith (TryF qfn k) $          -- log and ...
                    r (A pa (M.insert qfn k fa))) -- record the pkg choice
      ts
    go (GoalChoiceF      ts) a           =
      casePSQ ts
        (failWith (Failure EmptyGoalChoice))   -- empty goal choice is an internal error
        (\ k v _xs -> continueWith (Next k) (v a)) -- commit to the first goal choice

-- | Interface.
exploreTree :: Alternative m => Tree a -> m (Assignment, RevDepMap)
exploreTree t = explore t (A M.empty M.empty)

-- | Interface.
exploreTreeLog :: Tree a -> Log Message (Assignment, RevDepMap)
exploreTreeLog t = exploreLog t (A M.empty M.empty)
