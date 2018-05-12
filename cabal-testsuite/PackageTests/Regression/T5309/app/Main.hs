{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main (main) where

import Data.TCM.Memoized


main :: IO ()
main = generateMemoizedTransitionCostMatrix 5 (const (const 1)) `seq` return ()
