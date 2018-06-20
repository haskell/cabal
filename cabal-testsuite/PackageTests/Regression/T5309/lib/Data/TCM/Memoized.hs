-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TCM.Memoized
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.TCM.Memoized
  ( FFI.MemoizedCostMatrix
  , generateMemoizedTransitionCostMatrix
  , FFI.getMedianAndCost
  ) where

import qualified Data.TCM.Memoized.FFI as FFI


-- |
-- /O(n^2)/ where @n@ is the alphabet size.
--
-- Generate a memoized TCM by supplying the size of the symbol alphabet and the
-- generating function for unambiguous symbol change cost to produce a memoized
-- TCM. A memoized TCM computes all the costs and medians of unambiguous,
-- singleton symbol set transitions strictly when this function is invoked. A
-- memoized TCM calculates the cost and medians of ambiguous symbol sets in a
-- lazy, memoized manner.
--
-- *Note:* The collection of ambiguous symbols set transitions is the powerset of
-- the collection of unambiguous, singleton symbol sets. The lazy, memoization is
-- a requisite for efficient computation on any non-trivial alphabet size.
generateMemoizedTransitionCostMatrix
  :: Word                   -- ^ Alphabet size
  -> (Word -> Word -> Word) -- ^ Generating function
  -> FFI.MemoizedCostMatrix
generateMemoizedTransitionCostMatrix = FFI.getMemoizedCostMatrix

{-
-- Causes ambiguity with Data.TCM.(!)
(!) :: Exportable s => FFI.MemoizedCostMatrix -> (s, s) -> (s, Word)
(!) memo (x,y) = FFI.getMedianAndCost memo x y
-}
