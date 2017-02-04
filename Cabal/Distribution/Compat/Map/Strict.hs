{-# LANGUAGE CPP #-}

module Distribution.Compat.Map.Strict
    ( module X
#if MIN_VERSION_containers(0,5,0)
#else
    , insertWith
#endif
    ) where

#if MIN_VERSION_containers(0,5,0)
import Data.Map.Strict as X
#else
import Data.Map as X hiding (insertWith, insertWith')
import qualified Data.Map

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = Data.Map.insertWith'
#endif
