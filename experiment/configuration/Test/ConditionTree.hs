module Test.ConditionTree where

import ConditionTree
import Test.QuickCheck
import qualified Test.Laws as Laws
import Test.Poly (A, B, C, M)

import Control.Monad (liftM, liftM2, liftM3)

import qualified Data.Foldable as Foldable
import Data.Monoid (Sum(..))

instance (Arbitrary condition, Arbitrary body)
      => Arbitrary (ConditionTree condition body) where
  arbitrary = sized $ \n -> do
    k <- choose (0,n)
    liftM ConditionTree $
      sequence [ resize (n `div` k) arbitrary | _ <- [1..k] ]

  shrink (ConditionTree ns) = [ ConditionTree ns' | ns' <- shrink ns ]

instance (Arbitrary condition, Arbitrary body)
      => Arbitrary (ConditionNode condition body) where
  arbitrary = sized arbNode
    where
      arbNode n = frequency $
        [ (2, liftM Body arbitrary)
        ] ++ concat
        [ [ (1, liftM2 IfThen arbitrary arbTree1)
          , (1, liftM3 IfElse arbitrary arbTree2 arbTree2)
          ]
        | n > 0
        ]
        where
          arbTree1 = resize (n `div` 2) arbitrary
          arbTree2 = resize (n `div` 2) arbitrary

  shrink (Body   b)       = [ Body b' | b' <- shrink b ]
  shrink (IfThen c e1)    = (case e1 of ConditionTree x -> x)
                         ++ [ IfThen c' e1  | c'  <- shrink c  ]
                         ++ [ IfThen c  e1' | e1' <- shrink e1 ]
  shrink (IfElse c e1 e2) = (case e1 of ConditionTree x -> x)
                         ++ (case e2 of ConditionTree x -> x)
                         ++ [ IfElse c' e1  e2  | c'  <- shrink c  ]
                         ++ [ IfElse c  e1' e2  | e1' <- shrink e1 ]
                         ++ [ IfElse c  e1  e2' | e2' <- shrink e2 ]
  
prop_tree_fmap_1 :: ConditionTree C M -> Bool
prop_tree_fmap_1 = Laws.fmap_1

prop_tree_fmap_2 :: (A -> B) -> (M -> A) -> ConditionTree C M -> Bool
prop_tree_fmap_2 = Laws.fmap_2

prop_node_fmap_1 :: ConditionNode C M -> Bool
prop_node_fmap_1 = Laws.fmap_1

prop_node_fmap_2 :: (A -> B) -> (M -> A) -> ConditionNode C M -> Bool
prop_node_fmap_2 = Laws.fmap_2

prop_tree_foldable_1 :: ConditionTree C M -> Bool 
prop_tree_foldable_1 = Laws.foldable_1

prop_tree_foldable_2 :: (A -> B -> B) -> B -> ConditionTree C A -> Bool
prop_tree_foldable_2 = Laws.foldable_2

prop_node_foldable_1 :: ConditionNode C M -> Bool 
prop_node_foldable_1 = Laws.foldable_1

prop_node_foldable_2 :: (A -> B -> B) -> B -> ConditionNode C A -> Bool
prop_node_foldable_2 = Laws.foldable_2

prop_resolve_1 :: (C -> Bool) -> ConditionTree.ConditionTree C M -> Bool
prop_resolve_1 v t = resolve v t == flatten (simplify (Left . v) t)

prop_simplify_1 t = simplify Right t == t

size :: ConditionNode a b -> Int
size = length . Foldable.toList
