-- | Module using external dependency and mentioning it in haddocks
module B (b) where

import A

-- | Use 'a'
b :: Int
b = a
