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

-- | Type of the search tree. Inlining the choice nodes for now.
data Tree a =
    PChoice     QPN a           (PSQ I        (Tree a))
  | FChoice     QFN a Bool Bool (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial, second Bool whether it's manual
  | SChoice     QSN a Bool      (PSQ Bool     (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice                  (PSQ OpenGoal (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason
  deriving (Eq, Show)
  -- Above, a choice is called trivial if it clearly does not matter. The
  -- special case of triviality we actually consider is if there are no new
  -- dependencies introduced by this node.

instance Functor Tree where
  fmap  f (PChoice qpn i     xs) = PChoice qpn (f i)     (fmap (fmap f) xs)
  fmap  f (FChoice qfn i b m xs) = FChoice qfn (f i) b m (fmap (fmap f) xs)
  fmap  f (SChoice qsn i b   xs) = SChoice qsn (f i) b   (fmap (fmap f) xs)
  fmap  f (GoalChoice        xs) = GoalChoice            (fmap (fmap f) xs)
  fmap _f (Done    rdm         ) = Done    rdm
  fmap _f (Fail    cs fr       ) = Fail    cs fr

data FailReason = InconsistentInitialConstraints
                | Conflicting [Dep QPN]
                | CannotInstall
                | CannotReinstall
                | Shadowed
                | Broken
                | GlobalConstraintVersion VR
                | GlobalConstraintInstalled
                | GlobalConstraintSource
                | GlobalConstraintFlag
                | ManualFlag
                | BuildFailureNotInIndex PN
                | MalformedFlagChoice QFN
                | MalformedStanzaChoice QSN
                | EmptyGoalChoice
                | Backjump
  deriving (Eq, Show)

-- | Functor for the tree type.
data TreeF a b =
    PChoiceF    QPN a           (PSQ I        b)
  | FChoiceF    QFN a Bool Bool (PSQ Bool     b)
  | SChoiceF    QSN a Bool      (PSQ Bool     b)
  | GoalChoiceF                 (PSQ OpenGoal b)
  | DoneF       RevDepMap
  | FailF       (ConflictSet QPN) FailReason

out :: Tree a -> TreeF a (Tree a)
out (PChoice    p i     ts) = PChoiceF    p i     ts
out (FChoice    p i b m ts) = FChoiceF    p i b m ts
out (SChoice    p i b   ts) = SChoiceF    p i b   ts
out (GoalChoice         ts) = GoalChoiceF         ts
out (Done       x         ) = DoneF       x
out (Fail       c x       ) = FailF       c x

inn :: TreeF a (Tree a) -> Tree a
inn (PChoiceF    p i     ts) = PChoice    p i     ts
inn (FChoiceF    p i b m ts) = FChoice    p i b m ts
inn (SChoiceF    p i b   ts) = SChoice    p i b   ts
inn (GoalChoiceF         ts) = GoalChoice         ts
inn (DoneF       x         ) = Done       x
inn (FailF       c x       ) = Fail       c x

instance Functor (TreeF a) where
  fmap f (PChoiceF    p i     ts) = PChoiceF    p i     (fmap f ts)
  fmap f (FChoiceF    p i b m ts) = FChoiceF    p i b m (fmap f ts)
  fmap f (SChoiceF    p i b   ts) = SChoiceF    p i b   (fmap f ts)
  fmap f (GoalChoiceF         ts) = GoalChoiceF         (fmap f ts)
  fmap _ (DoneF       x         ) = DoneF       x
  fmap _ (FailF       c x       ) = FailF       c x

instance Foldable (TreeF a) where
  foldr op e (PChoiceF    _ _     ts) = foldr op e ts
  foldr op e (FChoiceF    _ _ _ _ ts) = foldr op e ts
  foldr op e (SChoiceF    _ _ _   ts) = foldr op e ts
  foldr op e (GoalChoiceF         ts) = foldr op e ts
  foldr _  e (DoneF       _         ) = e
  foldr _  e (FailF       _ _       ) = e

instance Traversable (TreeF a) where
  traverse f (PChoiceF    p i     ts) = PChoiceF    <$> pure p <*> pure i <*>                       traverse f ts
  traverse f (FChoiceF    p i b m ts) = FChoiceF    <$> pure p <*> pure i <*> pure b <*> pure m <*> traverse f ts
  traverse f (SChoiceF    p i b   ts) = SChoiceF    <$> pure p <*> pure i <*> pure b <*>            traverse f ts
  traverse f (GoalChoiceF         ts) = GoalChoiceF <$>                                             traverse f ts
  traverse _ (DoneF       x         ) = DoneF       <$> pure x
  traverse _ (FailF       c x       ) = FailF       <$> pure c <*> pure x

-- | Determines whether a tree is active, i.e., isn't a failure node.
active :: Tree a -> Bool
active (Fail _ _) = False
active _          = True

-- | Determines how many active choices are available in a node. Note that we
-- count goal choices as having one choice, always.
choices :: Tree a -> Int
choices (PChoice    _ _     ts) = P.length (P.filter active ts)
choices (FChoice    _ _ _ _ ts) = P.length (P.filter active ts)
choices (SChoice    _ _ _   ts) = P.length (P.filter active ts)
choices (GoalChoice         _ ) = 1
choices (Done       _         ) = 1
choices (Fail       _ _       ) = 0

-- | Variant of 'choices' that only approximates the number of choices,
-- using 'llength'.
lchoices :: Tree a -> Int
lchoices (PChoice    _ _     ts) = P.llength (P.filter active ts)
lchoices (FChoice    _ _ _ _ ts) = P.llength (P.filter active ts)
lchoices (SChoice    _ _ _   ts) = P.llength (P.filter active ts)
lchoices (GoalChoice         _ ) = 1
lchoices (Done       _         ) = 1
lchoices (Fail       _ _       ) = 0

-- | Catamorphism on trees.
cata :: (TreeF a b -> b) -> Tree a -> b
cata phi x = (phi . fmap (cata phi) . out) x

trav :: (TreeF a (Tree b) -> TreeF b (Tree b)) -> Tree a -> Tree b
trav psi x = cata (inn . psi) x

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
