module Foo where

import Data.Map

-- | A dummy function using 'Map'
f :: (a -> b) -> Map k a -> Map k b
f = fmap
