module Sizes (intSize) where

#include <limits.h>

-- | The size of a C int, filled in by hsc2hs.
intSize :: Int
intSize = #size int
