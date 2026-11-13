module Sizes (intSize, incr) where

import Foreign.C.Types (CInt (..))

#include <limits.h>

-- | The size of a C int, filled in by hsc2hs.
intSize :: Int
intSize = #size int

-- A C function that hsc2hs writes beside the module, with a header for it.
#def int sizes_incr(int x) { return x + 1; }

foreign import ccall "Sizes_hsc.h sizes_incr" incr :: CInt -> CInt
