module ConditionTree (
    ConditionTree(..),
    ConditionNode(..),
    
    flatten,
    leaves,
    unconditional,
    resolve,
    resolve',
    simplify,
    mapCondition,
    paths,
  ) where

import PartialValuation 
         ( PartialValuation(..), TotalValuation )

import Data.Monoid
         ( Monoid(..) )
import qualified Data.Foldable as Foldable
         ( Foldable(..), toList )
import Data.Foldable
         ( Foldable )

-- | A condition tree is a tree of values guarded by condition expressions.
-- Each level of the tree has a list of nodes.
--
newtype ConditionTree condition body
      = ConditionTree [ConditionNode condition body]
  deriving (Eq, Show)

-- An individual condition tree node is either a body value or a conditional
-- with a further tree below. Conditionals can have an else part.
--
data ConditionNode condition body
   = Body             body
   | IfThen condition (ConditionTree condition body)
   | IfElse condition (ConditionTree condition body)
                      (ConditionTree condition body)
  deriving (Eq, Show)


instance Functor (ConditionTree condition) where
  fmap f (ConditionTree ns) = ConditionTree (map (fmap f) ns)

instance Functor (ConditionNode condition) where
  fmap f (Body x)          = Body     (f x)
  fmap f (IfThen c  e)     = IfThen c (fmap f e)
  fmap f (IfElse c  e1 e2) = IfElse c (fmap f e1) (fmap f e2)

instance Foldable (ConditionTree condition) where
  foldr f z (ConditionTree ns) = foldr (flip (Foldable.foldr f)) z ns

instance Foldable (ConditionNode condition) where
  foldr f z (Body      b)     = f b z
  foldr f z (IfThen  _ e)     = Foldable.foldr f z e
  foldr f z (IfElse  _ e1 e2) = Foldable.foldr f (Foldable.foldr f z e2) e1

-- | Take all the leaves and flatten them into one value using the monoid
-- operation.
--
flatten :: Monoid body
        => ConditionTree condition body -> body
flatten = Foldable.fold

-- | Get a list of all the leaves of the tree.
--
leaves :: ConditionTree condition body -> [body]
leaves = Foldable.toList

-- | Get a list of all the body nodes that are unconditional
--
unconditional :: ConditionTree condition body -> [body]
unconditional (ConditionTree nodes) = [ b | Body b <- nodes ]

{-
-- | Within each level of the tree, merge all body nodes using the monoid
-- operation.
--
-- Note that this is only valid for commutative monoids
--
reorder :: ConditionTree condition body -> ConditionTree condition body
reorder (ConditionTree nodes) =
    ConditionTree (body:conditions)
  where
    body = mappend [ b | Body b <- nodes ]
    conditions = 
    -}
  

-- | Resolve a condition tree by evaluating all the conditionals and merging
-- together all those bodies for which the guard conditions are true.
--
-- This gives us the semantics of a 'PredicateExpr' so is crucial in the
-- definition of other properties.
--
resolve :: Monoid body
        => TotalValuation condition
        -> ConditionTree condition body -> body
resolve eval = flip evalTree mempty
  where
    evalTree (ConditionTree ns) accum = foldr evalNode accum ns

    evalNode (Body body) accum = body `mappend` accum

    evalNode (IfThen condition thenPart) accum
      | eval condition = evalTree thenPart accum
      | otherwise      = accum

    evalNode (IfElse condition thenPart elsePart) accum
      | eval condition = evalTree thenPart accum
      | otherwise      = evalTree elsePart accum

-- property: in all contexts,
--   IfThen c t === IfThenElse c t mempty

-- property: in all contexts,
--   [IfThenElse c t e] === [IfThen c t, IfThen (Not c) e]

-- property:
--   n === IfThen True [n]
--   IfThen False ns  === mempty

resolve' :: Monoid body
         => PartialValuation condition
         -> ConditionTree condition body -> Maybe body
resolve' peval = flip evalTree mempty
  where
    evalTree (ConditionTree ns) accum = foldr evalNode accum ns

    evalNode (Body body) accum = fmap (body `mappend`) accum

    evalNode (IfThen condition thenPart) accum =
      case applyPartialValuation peval condition of
        Just True  -> evalTree thenPart accum
        Just False -> accum
        Nothing    -> Nothing

    evalNode (IfElse condition thenPart elsePart) accum =
      case applyPartialValuation peval condition of
        Just True  -> evalTree thenPart accum
        Just False -> evalTree elsePart accum
        Nothing    -> Nothing

-- | Simplify a condition tree by trying to evaluate or simplify all the
-- conditional expressions. Whenever a conditional expression can be fully
-- evaluated then we resolve the conditonal. Otherwise the conditional
-- expression is just simplified and the conditional left unresolved.
--
-- * Property, simplify is an identity if no expressions are changed
--
-- > simplify Right = id
--
-- * Property, for all condition valuations v
--
-- > flatten . simplify (Left . v) = resolve v
--
simplify :: (condition -> Either Bool condition')
          -> ConditionTree condition  body
          -> ConditionTree condition' body
simplify simplifyCondition = simplifyTree
  where
    simplifyTree  ns = ConditionTree (simplifyTree' ns [])
    simplifyTree' (ConditionTree ns) ns' = foldr simplifyNode ns' ns

    simplifyNode (Body body) ns = Body body : ns

    simplifyNode (IfThen condition thenPart) ns =
      case simplifyCondition condition of
        Left  True       -> simplifyTree' thenPart ns
        Left  False      -> ns
        Right condition' -> IfThen condition' (simplifyTree thenPart) : ns

    simplifyNode (IfElse condition thenPart elsePart) ns =
      case simplifyCondition condition of
        Left  True       -> simplifyTree' thenPart ns
        Left  False      -> simplifyTree' elsePart ns
        Right condition' -> IfElse condition' (simplifyTree thenPart)
                                              (simplifyTree elsePart) : ns

-- property: simplify (Left . f) == ConditionTree [ns] where all ns isbody
-- that is, if we simplify every conditional we should end up with a (possibly
-- empty) list of bodies (no conditionals).

-- property: simplify Right = id

mapCondition :: (condition -> condition')
             -> ConditionTree condition  body
             -> ConditionTree condition' body
mapCondition f = simplify (Right . f)

-- property: canonical t => mapCondition id t = t

-- | All the leaves of the condition tree along with the conditions along the
-- path to that leaf.
--
-- Effectively this gives an alternative representation of the tree but without
-- any of the sharing of conditions that we get from the tree structure.
--
paths :: (condition -> condition)
      -> ConditionTree condition body
      -> [([condition], body)]
paths cnot = leaves . pathsTree []
  where
    pathsTree path (ConditionTree ns) = ConditionTree (map (pathsNode path) ns)

    pathsNode path (Body body) =
      Body (path, body)

    pathsNode path (IfThen condition thenPart) =
      IfThen condition (pathsTree (condition:path) thenPart)

    pathsNode path (IfElse condition thenPart elsePart) =
      IfElse condition (pathsTree (     condition:path) thenPart)
                       (pathsTree (cnot condition:path) elsePart)
