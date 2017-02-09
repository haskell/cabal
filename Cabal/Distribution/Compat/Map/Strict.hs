{-# LANGUAGE CPP #-}

-- For bootstrapping GHC
#ifdef MIN_VERSION_containers
#if MIN_VERSION_containers(0,5,0)
#define HAVE_containers_050
#endif
#endif

module Distribution.Compat.Map.Strict
    ( module X
#ifdef HAVE_containers_050
#else
    , insertWith
#endif
    ) where

#ifdef HAVE_containers_050
import Data.Map.Strict as X
#else
import Data.Map as X hiding (insertWith, insertWith')
import qualified Data.Map

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = Data.Map.insertWith'
#endif
