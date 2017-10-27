{-# LANGUAGE CPP #-}

-- For bootstrapping GHC
#ifdef MIN_VERSION_containers

#if MIN_VERSION_containers(0,5,0)
#define HAVE_containers_050
#endif

#elif __GLASGOW_HASKELL__ >= 706
#define HAVE_containers_050
#endif

module Distribution.Compat.Map.Strict
    ( module X
#ifndef HAVE_containers_050
    , insertWith
    , fromSet
#endif
    ) where

#ifdef HAVE_containers_050
import Data.Map.Strict as X
#else
import Data.Map as X hiding (insertWith, insertWith')
import qualified Data.Map (Map)
import qualified Data.Set (Set)

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = Data.Map.insertWith'

fromSet :: (k -> a) -> Data.Set.Set k -> Map k a
fromSet f = Data.Map.fromDistinctAscList . Prelude.map (\k -> (k, f k)) . Data.Set.toList
#endif
