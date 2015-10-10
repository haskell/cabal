module Test.PredicateExpr where

import PredicateExpr
import qualified PartialValuation
import PartialValuation (PartialValuation, TotalValuation)

import Test.PartialValuation

import Test.QuickCheck
import qualified Test.Laws as Laws
import Test.Poly (A, B, C, M)

import Text.Show.Functions

import Control.Monad (MonadPlus(..), liftM, liftM2)
import qualified Data.Foldable as Foldable
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))

instance Arbitrary base => Arbitrary (PredicateExpr base) where
  arbitrary = sized arbExp
    where
      arbExp n = frequency $
        [ (2, liftM PredExprBase arbitrary)
        , (1, liftM PredExprLit  arbitrary)
        ] ++ concat
        [ [ (2, liftM  PredExprNot arbExp1)
          , (2, liftM2 PredExprAnd arbExp2 arbExp2)
          , (2, liftM2 PredExprOr  arbExp2 arbExp2)
          ]
        | n > 0
        ]
        where
          arbExp1 = arbExp (n-1)
          arbExp2 = arbExp (n `div` 2)

  shrink (PredExprBase base) = [ PredExprBase base' | base' <- shrink base ]
  shrink (PredExprLit  lit)  = []
  shrink (PredExprNot e    ) = e 
                             : [ PredExprNot e' | e' <- shrink e ]
  shrink (PredExprAnd e1 e2) = [e1, e2]
                            ++ [ PredExprAnd e1' e2 | e1' <- shrink e1 ]
                            ++ [ PredExprAnd e1 e2' | e2' <- shrink e2 ]
  shrink (PredExprOr  e1 e2) = [e1, e2]
                            ++ [ PredExprOr e1' e2 | e1' <- shrink e1 ]
                            ++ [ PredExprOr e1 e2' | e2' <- shrink e2 ]

-- fold is an identity, given suitable constructors as args
prop_fold_id :: PredicateExpr A -> Bool
prop_fold_id x = predId x == x
  where predId = PredicateExpr.fold PredExprBase PredExprLit
                                    PredExprNot PredExprAnd PredExprOr

-- Check basePredicates against an oracle that's obviously(!) right.
prop_basePredicates_1 :: PredicateExpr A -> Bool
prop_basePredicates_1 x = basePredicates x == basePredicates' x 
  where
    basePredicates' = PredicateExpr.fold (\x -> [x]) (const []) id (++) (++)

-- PredicateExpr is in Functor, check the functor laws
prop_fmap_1 :: PredicateExpr A -> Bool
prop_fmap_1 = Laws.fmap_1

prop_fmap_2 :: (B -> C) -> (A -> B) -> PredicateExpr A -> Bool
prop_fmap_2 = Laws.fmap_2

-- PredicateExpr is in Foldable, check the laws
prop_foldable_1 :: PredicateExpr M -> Bool 
prop_foldable_1 = Laws.foldable_1

prop_foldable_2 :: (A -> B -> B) -> B -> PredicateExpr A -> Bool
prop_foldable_2 = Laws.foldable_2

-- with a total valuation for the base predicate, eval' is the same as eval
prop_eval'_1 :: TotalValuation A -> PredicateExpr A -> Bool
prop_eval'_1 v e = eval' (Just . v) e == Just (eval v e)

-- the overall evaluation of the expression is Nothing if and only if the
-- base valuation is Nothing on any base value in the expression
prop_eval'_2 :: (A -> Maybe Bool) -> PredicateExpr A -> Bool
prop_eval'_2 v e = (eval' v e == Nothing)
                == any (\e' -> v e' == Nothing) (basePredicates e)

prop_simplify_1 :: PartialValuation A -> TotalValuation A -> PredicateExpr A -> Bool
prop_simplify_1 v' v e = eval (PartialValuation.extend' v' v) e
                      == eval v (simplify v' e)
  where
    extend f' f = \x -> applyPartialValuation f' x `override` f x
    override    = flip fromMaybe

prop_simplify_2 :: TotalValuation A -> PredicateExpr A -> Bool
prop_simplify_2 v e = eval v (simplify mempty e) == eval v e

prop_simplify_3 :: TotalValuation A -> PredicateExpr A -> Bool
prop_simplify_3 v e = simplify v' e == PredExprLit (eval v e)
  where
    v' = PartialValuation.liftTotal v

prop_simplify_4 :: PredicateExpr A -> Bool
prop_simplify_4 e = not (literalAsSubExpr (simplify PartialValuation.null e))
  where
    literalAsSubExpr (PredExprLit _) = False
    literalAsSubExpr e = PredicateExpr.fold (const False) (const True)
                                            id (||) (||) e
-- idempotentcy, at least for a partial valuation that keeps the old expression
-- when it gives it no value. In other words simplify does all it's work in one
-- go.
prop_simplify_5 :: PartialValuation A -> PredicateExpr A -> Bool
prop_simplify_5 v e = simplify v (simplify v e) == simplify v e
