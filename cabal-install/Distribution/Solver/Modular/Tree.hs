{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Solver.Modular.Tree
    ( FailReason(..)
    , POption(..)
    , Tree(..)
    , TreeF(..)
    , Weight
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

import Distribution.Solver.Modular.Degree
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.PSQ (PSQ)
import Distribution.Solver.Modular.Version
import Distribution.Solver.Modular.WeightedPSQ (WeightedPSQ)
import qualified Distribution.Solver.Modular.WeightedPSQ as W
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackagePath

type Weight = Double

-- | Type of the search tree. Inlining the choice nodes for now. Weights on
-- package, flag, and stanza choices control the traversal order.
-- TODO: The weight type should be changed from [Double] to Double to avoid
-- giving too much weight to preferences that are applied later.
data Tree a =
    -- | Choose a version for a package (or choose to link)
    PChoice QPN a (WeightedPSQ [Weight] POption (Tree a))

    -- | Choose a value for a flag
    --
    -- The Bool indicates whether it's manual.
  | FChoice QFN a WeakOrTrivial Bool (WeightedPSQ [Weight] Bool (Tree a))

    -- | Choose whether or not to enable a stanza
  | SChoice QSN a WeakOrTrivial (WeightedPSQ [Weight] Bool (Tree a))

    -- | Choose which choice to make next
    --
    -- Invariants:
    --
    -- * PSQ should never be empty
    -- * For each choice we additionally record the 'QGoalReason' why we are
    --   introducing that goal into tree. Note that most of the time we are
    --   working with @Tree QGoalReason@; in that case, we must have the
    --   invariant that the 'QGoalReason' cached in the 'PChoice', 'FChoice'
    --   or 'SChoice' directly below a 'GoalChoice' node must equal the reason
    --   recorded on that 'GoalChoice' node.
  | GoalChoice (PSQ (Goal QPN) (Tree a))

    -- | We're done -- we found a solution!
  | Done RevDepMap

    -- | We failed to find a solution in this path through the tree
  | Fail (ConflictSet QPN) FailReason
  deriving (Eq, Show, Functor)

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
-- vice versa). It means that @1.P@ and @0.P@ really must be the very same package
-- (and hence must have the same build time configuration, and their
-- dependencies must also be the exact same).
--
-- See <http://www.well-typed.com/blog/2015/03/qualified-goals/> for details.
data POption = POption I (Maybe PackagePath)
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
    PChoiceF    QPN a                    (WeightedPSQ [Weight] POption b)
  | FChoiceF    QFN a WeakOrTrivial Bool (WeightedPSQ [Weight] Bool    b)
  | SChoiceF    QSN a WeakOrTrivial      (WeightedPSQ [Weight] Bool    b)
  | GoalChoiceF                          (PSQ (Goal QPN) b)
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
choices (PChoice    _ _     ts) = W.length (W.filter active ts)
choices (FChoice    _ _ _ _ ts) = W.length (W.filter active ts)
choices (SChoice    _ _ _   ts) = W.length (W.filter active ts)
choices (GoalChoice         _ ) = 1
choices (Done       _         ) = 1
choices (Fail       _ _       ) = 0

-- | Variant of 'choices' that only approximates the number of choices.
dchoices :: Tree a -> Degree
dchoices (PChoice    _ _     ts) = W.degree (W.filter active ts)
dchoices (FChoice    _ _ _ _ ts) = W.degree (W.filter active ts)
dchoices (SChoice    _ _ _   ts) = W.degree (W.filter active ts)
dchoices (GoalChoice         _ ) = ZeroOrOne
dchoices (Done       _         ) = ZeroOrOne
dchoices (Fail       _ _       ) = ZeroOrOne

-- | Variant of 'choices' that only approximates the number of choices.
zeroOrOneChoices :: Tree a -> Bool
zeroOrOneChoices (PChoice    _ _     ts) = W.isZeroOrOne (W.filter active ts)
zeroOrOneChoices (FChoice    _ _ _ _ ts) = W.isZeroOrOne (W.filter active ts)
zeroOrOneChoices (SChoice    _ _ _   ts) = W.isZeroOrOne (W.filter active ts)
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
