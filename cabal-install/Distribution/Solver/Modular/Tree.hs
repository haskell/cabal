{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Distribution.Solver.Modular.Tree
    ( FailReason(..)
    , POption(..)
    , Tree(..)
    , TreeF(..)
    , Weight
    , ana
    , cata
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
--
-- The tree can hold additional data on 'Done' nodes (type 'd') and choice nodes
-- (type 'c'). For example, during the final traversal, choice nodes contain the
-- variables that introduced the choices, and 'Done' nodes contain the
-- assignments for all variables.
--
-- TODO: The weight type should be changed from [Double] to Double to avoid
-- giving too much weight to preferences that are applied later.
data Tree d c =
    -- | Choose a version for a package (or choose to link)
    PChoice QPN c (WeightedPSQ [Weight] POption (Tree d c))

    -- | Choose a value for a flag
    --
    -- The Bool indicates whether it's manual.
  | FChoice QFN c WeakOrTrivial Bool (WeightedPSQ [Weight] Bool (Tree d c))

    -- | Choose whether or not to enable a stanza
  | SChoice QSN c WeakOrTrivial (WeightedPSQ [Weight] Bool (Tree d c))

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
  | GoalChoice (PSQ (Goal QPN) (Tree d c))

    -- | We're done -- we found a solution!
  | Done RevDepMap d

    -- | We failed to find a solution in this path through the tree
  | Fail ConflictSet FailReason
  deriving (Eq, Show)

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

-- | Functor for the tree type. 'a' is the type of nodes' children. 'd' and 'c'
-- have the same meaning as in 'Tree'.
data TreeF d c a =
    PChoiceF    QPN c                    (WeightedPSQ [Weight] POption a)
  | FChoiceF    QFN c WeakOrTrivial Bool (WeightedPSQ [Weight] Bool    a)
  | SChoiceF    QSN c WeakOrTrivial      (WeightedPSQ [Weight] Bool    a)
  | GoalChoiceF                          (PSQ (Goal QPN) a)
  | DoneF       RevDepMap d
  | FailF       ConflictSet FailReason
  deriving (Functor, Foldable, Traversable)

out :: Tree d c -> TreeF d c (Tree d c)
out (PChoice    p i     ts) = PChoiceF    p i     ts
out (FChoice    p i b m ts) = FChoiceF    p i b m ts
out (SChoice    p i b   ts) = SChoiceF    p i b   ts
out (GoalChoice         ts) = GoalChoiceF         ts
out (Done       x s       ) = DoneF       x s
out (Fail       c x       ) = FailF       c x

inn :: TreeF d c (Tree d c) -> Tree d c
inn (PChoiceF    p i     ts) = PChoice    p i     ts
inn (FChoiceF    p i b m ts) = FChoice    p i b m ts
inn (SChoiceF    p i b   ts) = SChoice    p i b   ts
inn (GoalChoiceF         ts) = GoalChoice         ts
inn (DoneF       x s       ) = Done       x s
inn (FailF       c x       ) = Fail       c x

innM :: Monad m => TreeF d c (m (Tree d c)) -> m (Tree d c)
innM (PChoiceF    p i     ts) = liftM (PChoice    p i    ) (sequence ts)
innM (FChoiceF    p i b m ts) = liftM (FChoice    p i b m) (sequence ts)
innM (SChoiceF    p i b   ts) = liftM (SChoice    p i b  ) (sequence ts)
innM (GoalChoiceF         ts) = liftM (GoalChoice        ) (sequence ts)
innM (DoneF       x s       ) = return $ Done     x s
innM (FailF       c x       ) = return $ Fail     c x

-- | Determines whether a tree is active, i.e., isn't a failure node.
active :: Tree d c -> Bool
active (Fail _ _) = False
active _          = True

-- | Approximates the number of active choices that are available in a node.
-- Note that we count goal choices as having one choice, always.
dchoices :: Tree d c -> Degree
dchoices (PChoice    _ _     ts) = W.degree (W.filter active ts)
dchoices (FChoice    _ _ _ _ ts) = W.degree (W.filter active ts)
dchoices (SChoice    _ _ _   ts) = W.degree (W.filter active ts)
dchoices (GoalChoice         _ ) = ZeroOrOne
dchoices (Done       _ _       ) = ZeroOrOne
dchoices (Fail       _ _       ) = ZeroOrOne

-- | Variant of 'dchoices' that traverses fewer children.
zeroOrOneChoices :: Tree d c -> Bool
zeroOrOneChoices (PChoice    _ _     ts) = W.isZeroOrOne (W.filter active ts)
zeroOrOneChoices (FChoice    _ _ _ _ ts) = W.isZeroOrOne (W.filter active ts)
zeroOrOneChoices (SChoice    _ _ _   ts) = W.isZeroOrOne (W.filter active ts)
zeroOrOneChoices (GoalChoice         _ ) = True
zeroOrOneChoices (Done       _ _       ) = True
zeroOrOneChoices (Fail       _ _       ) = True

-- | Catamorphism on trees.
cata :: (TreeF d c a -> a) -> Tree d c -> a
cata phi x = (phi . fmap (cata phi) . out) x

trav :: (TreeF d c (Tree d a) -> TreeF d a (Tree d a)) -> Tree d c -> Tree d a
trav psi x = cata (inn . psi) x

-- | Paramorphism on trees.
para :: (TreeF d c (a, Tree d c) -> a) -> Tree d c -> a
para phi = phi . fmap (\ x -> (para phi x, x)) . out

-- | Anamorphism on trees.
ana :: (a -> TreeF d c a) -> a -> Tree d c
ana psi = inn . fmap (ana psi) . psi
