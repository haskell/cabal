{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}

module Distribution.Compat.Map
  (
  alter
  )
 where

import Data.Map as Map

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 605
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k m = case Map.lookup k m of
              Nothing -> case f Nothing of
                         Nothing -> m
                         Just v  -> insert k v m
              j ->  case f j of
                    Nothing -> delete k m
                    Just v -> insert k v m
#endif

