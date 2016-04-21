{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Client.Dependency.Modular.Tree
    ( FailReason(..)
    , POption(..)
    , Tree(..)
    , TreeF(..)
    , ana
    , cata
    , choices
    , dchoices
    , inn
    , innM
    , para
    , trav
    , zeroOrOneChoices
    ) where

import Control.Monad hiding (mapM, sequence)
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr, mapM, sequence)

import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ (PSQ)
import qualified Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Version
import Distribution.Client.Dependency.Types ( ConstraintSource(..) )

-- | Type of the search tree. Inlining the choice nodes for now.
data Tree a =
    PChoice     QPN a           (PSQ POption       (Tree a))
  | FChoice     QFN a Bool Bool (PSQ Bool          (Tree a)) -- Bool indicates whether it's weak/trivial, second Bool whether it's manual
  | SChoice     QSN a Bool      (PSQ Bool          (Tree a)) -- Bool indicates whether it's trivial
  | GoalChoice                  (PSQ (OpenGoal ()) (Tree a)) -- PSQ should never be empty
  | Done        RevDepMap
  | Fail        (ConflictSet QPN) FailReason
  deriving (Eq, Show, Functor)
  -- Above, a choice is called trivial if it clearly does not matter. The
  -- special case of triviality we actually consider is if there are no new
  -- dependencies introduced by this node.
  --
  -- A (flag) choice is called weak if we do want to defer it. This is the
  -- case for flags that should be implied by what's currently installed on
  -- the system, as opposed to flags that are used to explicitly enable or
  -- disable some functionality.

-- | A package option is a package instance with an optional linking annotation
--
-- The modular solver has a number of package goals to solve for, and can only
-- pick a single package version for a single goal. In order to allow to
-- install multiple versions of the same package as part of a single solution
-- the solver uses qualified goals. For example, @0.P@ and @1.P@ might both
-- be qualified goals for @P@, allowing to pick a difference version of package
-- @P@ for @0.P@ and @1.P@.
--
-- Linking is an essential part of this story. In addition to picking a specific
-- version for @1.P@, the solver can also decide to link @1.P@ to @0.P@ (or
-- vice versa). Teans that @1.P@ and @0.P@ really must be the very same package
-- (and hence must have the same build time configuration, and their
-- dependencies must also be the exact same).
--
-- See <http://www.well-typed.com/blog/2015/03/qualified-goals/> for details.
data POption = POption I (Maybe PP)
  deriving (Eq, Show)

data FailReason = InconsistentInitialConstraints
                | Conflicting [Dep QPN]
                | CannotInstall
                | CannotReinstall
                | Shadowed
                | Broken
                | GlobalConstraintVersion VR ConstraintSource
                | GlobalConstraintInstalled ConstraintSource
                | GlobalConstraintSource ConstraintSource
                | GlobalConstraintFlag ConstraintSource
                | ManualFlag
                | MalformedFlagChoice QFN
                | MalformedStanzaChoice QSN
                | EmptyGoalChoice
                | Backjump
                | MultipleInstances
                | DependenciesNotLinked String
                | CyclicDependencies
  deriving (Eq, Show)

-- | Functor for the tree type.
data TreeF a b =
    PChoiceF    QPN a           (PSQ POption       b)
  | FChoiceF    QFN a Bool Bool (PSQ Bool          b)
  | SChoiceF    QSN a Bool      (PSQ Bool          b)
  | GoalChoiceF                 (PSQ (OpenGoal ()) b)
  | DoneF       RevDepMap
  | FailF       (ConflictSet QPN) FailReason
  deriving (Functor, Foldable, Traversable)

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

innM :: Monad m => TreeF a (m (Tree a)) -> m (Tree a)
innM (PChoiceF    p i     ts) = liftM (PChoice    p i    ) (sequence ts)
innM (FChoiceF    p i b m ts) = liftM (FChoice    p i b m) (sequence ts)
innM (SChoiceF    p i b   ts) = liftM (SChoice    p i b  ) (sequence ts)
innM (GoalChoiceF         ts) = liftM (GoalChoice        ) (sequence ts)
innM (DoneF       x         ) = return $ Done     x
innM (FailF       c x       ) = return $ Fail     c x

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

-- | Variant of 'choices' that only approximates the number of choices.
dchoices :: Tree a -> P.Degree
dchoices (PChoice    _ _     ts) = P.degree (P.filter active ts)
dchoices (FChoice    _ _ _ _ ts) = P.degree (P.filter active ts)
dchoices (SChoice    _ _ _   ts) = P.degree (P.filter active ts)
dchoices (GoalChoice         _ ) = P.ZeroOrOne
dchoices (Done       _         ) = P.ZeroOrOne
dchoices (Fail       _ _       ) = P.ZeroOrOne

-- | Variant of 'choices' that only approximates the number of choices.
zeroOrOneChoices :: Tree a -> Bool
zeroOrOneChoices (PChoice    _ _     ts) = P.isZeroOrOne (P.filter active ts)
zeroOrOneChoices (FChoice    _ _ _ _ ts) = P.isZeroOrOne (P.filter active ts)
zeroOrOneChoices (SChoice    _ _ _   ts) = P.isZeroOrOne (P.filter active ts)
zeroOrOneChoices (GoalChoice         _ ) = True
zeroOrOneChoices (Done       _         ) = True
zeroOrOneChoices (Fail       _ _       ) = True

-- | Catamorphism on trees.
cata :: (TreeF a b -> b) -> Tree a -> b
cata phi x = (phi . fmap (cata phi) . out) x

trav :: (TreeF a (Tree b) -> TreeF b (Tree b)) -> Tree a -> Tree b
trav psi x = cata (inn . psi) x

-- | Paramorphism on trees.
para :: (TreeF a (b, Tree a) -> b) -> Tree a -> b
para phi = phi . fmap (\ x -> (para phi x, x)) . out

-- | Anamorphism on trees.
ana :: (b -> TreeF a b) -> b -> Tree a
ana psi = inn . fmap (ana psi) . psi
