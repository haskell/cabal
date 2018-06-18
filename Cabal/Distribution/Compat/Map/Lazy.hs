{-# LANGUAGE CPP #-}

-- For bootstrapping GHC
#ifdef MIN_VERSION_containers
#if MIN_VERSION_containers(0,5,0)
#define HAVE_containers_050
#endif
#endif

module Distribution.Compat.Map.Lazy
    ( module X ) where

#ifdef HAVE_containers_050
import Data.Map.Lazy as X
#else
import Data.Map as X
import qualified Data.Map
import qualified Data.Set

fromSet :: (k -> a) -> Data.Set.Set k -> Map k a
fromSet f = Data.Map.fromDistinctAscList . Prelude.map (\k -> (k, f k)) . Data.Set.toList
#endif
