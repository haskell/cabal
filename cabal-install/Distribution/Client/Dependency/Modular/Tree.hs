module Distribution.Client.Dependency.Modular.Tree where

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr, mapM)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Version

-- | Goals are qualified flagged dependencies, together with a reason for
-- their presence.
data Goal = Goal (FlaggedDep QPN) GoalReason
  deriving (Eq, Show)

-- | Reasons why a goal can be added to a goal set.
data GoalReason = UserGoal | PDependency (PI QPN) | FDependency QFN Bool
  deriving (Eq, Show)

goalReasonToVars :: GoalReason -> [Var QPN]
goalReasonToVars UserGoal                 = []
goalReasonToVars (PDependency (PI qpn _)) = [P qpn]
goalReasonToVars (FDependency qfn _)      = [F qfn]

-- | Type of the search tree. Inlining the choice nodes for now.
data Tree a =
    PChoice     QPN a      (PSQ I    (Tree a))
  | FChoice     QFN a Bool (PSQ Bool (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice      a      (PSQ Goal (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        [Var QPN] FailReason
  deriving (Eq, Show)

data FailReason = InconsistentInitialConstraints
                | Conflicting (Dep QPN)
                | ConflictingFlag
                | CannotInstall
                | CannotReinstall
                | GlobalConstraintVersion VR
                | GlobalConstraintInstalled
                | GlobalConstraintSource
                | GlobalConstraintFlag
                | BuildFailureNotInIndex PN
                | EmptyGoalChoice
  deriving (Eq, Show)

-- | Functor for the tree type.
data TreeF a b =
    PChoiceF    QPN a      (PSQ I    b)
  | FChoiceF    QFN a Bool (PSQ Bool b)
  | GoalChoiceF     a      (PSQ Goal b)
  | DoneF       RevDepMap
  | FailF       [Var QPN] FailReason

out :: Tree a -> TreeF a (Tree a)
out (PChoice    p i   ts) = PChoiceF    p i   ts
out (FChoice    p i b ts) = FChoiceF    p i b ts
out (GoalChoice   i   ts) = GoalChoiceF   i   ts
out (Done       x       ) = DoneF       x
out (Fail       c x     ) = FailF       c x

inn :: TreeF a (Tree a) -> (Tree a)
inn (PChoiceF    p i   ts) = PChoice    p i   ts
inn (FChoiceF    p i b ts) = FChoice    p i b ts
inn (GoalChoiceF   i   ts) = GoalChoice   i   ts
inn (DoneF       x       ) = Done       x
inn (FailF       c x     ) = Fail       c x

instance Functor (TreeF a) where
  fmap f (PChoiceF    p i   ts) = PChoiceF    p i   (fmap f ts)
  fmap f (FChoiceF    p i b ts) = FChoiceF    p i b (fmap f ts)
  fmap f (GoalChoiceF   i   ts) = GoalChoiceF   i   (fmap f ts)
  fmap _ (DoneF       x       ) = DoneF       x
  fmap _ (FailF       c x     ) = FailF       c x

instance Foldable (TreeF a) where
  foldr op e (PChoiceF    _ _   ts) = foldr op e ts
  foldr op e (FChoiceF    _ _ _ ts) = foldr op e ts
  foldr op e (GoalChoiceF   _   ts) = foldr op e ts
  foldr _  e (DoneF       _       ) = e
  foldr _  e (FailF       _ _     ) = e

instance Traversable (TreeF a) where
  traverse f (PChoiceF    p i   ts) = PChoiceF    <$> pure p <*> pure i <*>            traverse f ts
  traverse f (FChoiceF    p i b ts) = FChoiceF    <$> pure p <*> pure i <*> pure b <*> traverse f ts
  traverse f (GoalChoiceF   i   ts) = GoalChoiceF <$>            pure i <*>            traverse f ts
  traverse _ (DoneF       x       ) = DoneF       <$> pure x
  traverse _ (FailF       c x     ) = FailF       <$> pure c <*> pure x

-- | Determines whether a tree is active, i.e., isn't a failure node.
active :: Tree a -> Bool
active (Fail _ _) = False
active _          = True

-- | Determines how many active choices are available in a node. Note that we
-- count goal choices as having one choice, always.
choices :: Tree a -> Int
choices (PChoice    _ _   ts) = P.length (P.filter active ts)
choices (FChoice    _ _ _ ts) = P.length (P.filter active ts)
choices (GoalChoice   _   _ ) = 1
choices (Done       _       ) = 1
choices (Fail       _ _     ) = 0

-- | Variant of 'choices' that only approximates the number of choices,
-- using 'llength'.
lchoices :: Tree a -> Int
lchoices (PChoice    _ _   ts) = P.llength (P.filter active ts)
lchoices (FChoice    _ _ _ ts) = P.llength (P.filter active ts)
lchoices (GoalChoice   _   _ ) = 1
lchoices (Done       _       ) = 1
lchoices (Fail       _ _     ) = 0

-- | Catamorphism on trees.
cata :: (TreeF a b -> b) -> Tree a -> b
cata phi = phi . fmap (cata phi) . out

-- | Paramorphism on trees.
para :: (TreeF a (b, Tree a) -> b) -> Tree a -> b
para phi = phi . fmap (\ x -> (para phi x, x)) . out

cataM :: Monad m => (TreeF a b -> m b) -> Tree a -> m b
cataM phi = phi <=< mapM (cataM phi) <=< return . out

-- | Anamorphism on trees.
ana :: (b -> TreeF a b) -> b -> Tree a
ana psi = inn . fmap (ana psi) . psi

anaM :: Monad m => (b -> m (TreeF a b)) -> b -> m (Tree a)
anaM psi = return . inn <=< mapM (anaM psi) <=< psi
