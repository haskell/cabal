module PredicateExpr (

  PredicateExpr(..),
  fold,

  eval, eval',
  simplify, simplify',

  basePredicates,


  PartialValuation(..),
  ) where

import PartialValuation

import Data.Monoid
         ( Monoid(..) )
import Control.Monad
         ( liftM, liftM2 )
import qualified Data.Foldable as Foldable
         ( Foldable(foldMap, foldr), toList )

-- | A predicate expression. It is parameterised over the type of the base
-- predicate.
--
data PredicateExpr base
   = PredExprBase base
   | PredExprLit  Bool
   | PredExprNot  (PredicateExpr base)
   | PredExprAnd  (PredicateExpr base) (PredicateExpr base)
   | PredExprOr   (PredicateExpr base) (PredicateExpr base)
  deriving (Eq, Show)

-- | The natural fold on the 'PredicateExpr' type.
--
-- For example, an evaluation function is just:
--
-- > eval :: PredicateExpr BasePredicate -> Bool
-- > eval = fold BasePredicate.eval id not (&&) (||)
--
fold :: (base -> a) -> (Bool -> a)
     -> (a -> a) -> (a -> a -> a) -> (a -> a -> a)
     -> PredicateExpr base -> a
fold fbase flit fnot fand for = fold'
  where
    fold' (PredExprBase base) = fbase base
    fold' (PredExprLit  lit)  = flit  lit
    fold' (PredExprNot  a)    = fnot  (fold' a)
    fold' (PredExprAnd  a b)  = fand  (fold' a) (fold' b)
    fold' (PredExprOr   a b)  = for   (fold' a) (fold' b)
{-# INLINE fold #-}

instance Functor PredicateExpr where
  fmap f = fold (PredExprBase . f)
                PredExprLit PredExprNot PredExprAnd PredExprOr

instance Foldable.Foldable PredicateExpr where
  foldMap f = fold f (const mempty) id mappend mappend

  foldr f z (PredExprBase b)  = f b z
  foldr f z (PredExprLit _)   = z
  foldr f z (PredExprNot a)   = Foldable.foldr f z a
  foldr f z (PredExprAnd a b) = Foldable.foldr f (Foldable.foldr f z b) a
  foldr f z (PredExprOr  a b) = Foldable.foldr f (Foldable.foldr f z b) a

-- | List all the base predicates occuring in the expression.
--
basePredicates :: PredicateExpr base -> [base]
basePredicates = Foldable.toList

-- | Given a valuation on the base predicate, extend it to a valuation on the
-- whole 'PredicateExpr'. This gives the semantics of a 'PredicateExpr'
--
eval :: TotalValuation base
     -> TotalValuation (PredicateExpr base)
eval evalBase = fold evalBase id not (&&) (||)

-- | Given a partial valuation on the base predicate, extend it to a partial
-- valuation on the whole 'PredicateExpr'.
--
-- * Property: with a total valuation for the base predicate, 'eval'' is the
-- same as 'eval':
--
-- > eval' (Just . v) = Just . eval v
--
-- * Property: the evaluation is 'Nothing' if and only if the valuation is
-- 'Nothing' on any value in the expression
--
-- >   eval' v e == Nothing
-- > = any (\e' -> v e' == Nothing) (basePredicates e)
--
eval' :: PartialValuation base
      -> PartialValuation (PredicateExpr base)
eval' (PartialValuation evalBase) = PartialValuation $
  fold evalBase return (liftM not) (liftM2 (&&)) (liftM2 (||))

-- | Simplify a 'PredicateExpr', given a function to evaluate or simplify
-- base predicates. It can do some simple algebraic simplification even if no
-- base predicates are evaluated.
--
-- * Property: simplify is the syntactic equivalent of evaluation. For any
-- partial valuation, using that to syntacticly simplify gives an expression
-- with the same set of valuations as the original expression with the partial
-- valuation extend to a full valuation.
--
-- > eval v (simplify (toEither v') e) = eval (extend v' v) e
-- >   where
-- >     extend f' f = \x -> f' x `override` f x
-- >     override    = flip fromMaybe
-- >
-- >     toEither  f = \x -> maybe (Right x) Left (f x)
-- >     toEither :: (b -> Maybe  a  )
-- >              -> (b -> Either a b)
--
-- * Property: when doing no evaluation of the base predicate, simplify does
-- not change the valuations of the expression. Or to put it another way,
-- @simplify Right@ is an identify function on the semantics of expressions.
-- Note this property is a simple consequence of the first property. If we pick
-- @v' x = Nothing@ then we can derive this property.
--
-- > eval v (simplify Right e) = eval v e
--
-- * Property: when doing full evaluation of the base predicate, simplify does
-- sufficient simplification to be equivalent to full evaluation using 'eval'.
--
-- > simplify (Left . v) e == PredExprLit (eval v e)
--
-- * Property: when doing no evaluation of the base predicate, simplify
-- eliminates all True and False literals occuring as subexpressions.
--
-- > not (literalAsSubExpr (simplify Right e))
-- >   where
-- >     literalAsSubExpr (PredExprLit _) = False
-- >     literalAsSubExpr e = PredicateExpr.fold (const False) (const True)
-- >                                             id (||) (||) e
--
-- The first property tells us simplify does correct simplification though
-- not necessarily that it does any evaluation at all. The third and fourth
-- properties tell us that it's doing enough simplification. Note that due to
-- the symmetry of '&&' and '||', the second property is not enough to tell us
-- we're doing full simplification. Full evaluation only needs to reduce on one
-- side of '&&' and '||', not on both. The third property shows we're
-- simplifying on both sides.
--
-- * Property: idempotentcy, at least for a partial valuation that keeps the
-- old expression when it gives it no value. In other words 'simplify' does
-- all it's work in one go.
--
-- > simplify v' . simplify v' = simplify v'
-- >   where
-- >     v' = toEither v
-- >     toEither  f x = maybe (Right x) Left (f x)
-- >     toEither :: (b -> Maybe  a  )
-- >              -> (b -> Either a b)
--
simplify :: PartialValuation base
         -> PredicateExpr base
         -> PredicateExpr base
simplify v = simplify' v'
  where
    v' x = maybe (Right x) Left (applyPartialValuation v x)

simplify' :: (base -> Either Bool base')
          -> PredicateExpr base
          -> PredicateExpr base'
simplify' valuation =
  fold simplifyBase simplifyLit
       simplifyNot simplifyAnd simplifyOr
  where
    simplifyBase predicate =
      case valuation predicate of
        Left  bool       -> PredExprLit  bool
        Right predicate' -> PredExprBase predicate'

    simplifyLit lit   = PredExprLit lit

    simplifyNot (PredExprLit lit) = PredExprLit (not lit)
    simplifyNot (PredExprNot e)   = e
    simplifyNot e                 = PredExprNot e

    simplifyAnd (PredExprLit True)  e2 = e2
    simplifyAnd (PredExprLit False) _  = PredExprLit False
    simplifyAnd e1 (PredExprLit True)  = e1
    simplifyAnd _  (PredExprLit False) = PredExprLit False
    simplifyAnd e1 e2                  = PredExprAnd e1 e2

    simplifyOr (PredExprLit True)  _  = PredExprLit True
    simplifyOr (PredExprLit False) e2 = e2
    simplifyOr _  (PredExprLit True)  = PredExprLit True
    simplifyOr e1 (PredExprLit False) = e1
    simplifyOr e1 e2                  = PredExprOr e1 e2
