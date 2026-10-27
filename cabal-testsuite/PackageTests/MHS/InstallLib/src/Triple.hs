module Triple (triple) where

import Foreign.C.Types (CInt (..))

foreign import ccall "triple.h triple" triple :: CInt -> CInt
