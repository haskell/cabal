module Twice (twice) where

import Foreign.C.Types (CInt (..))

foreign import ccall "twice.h csources_twice" twice :: CInt -> CInt
