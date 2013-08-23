{-# LANGUAGE CPP #-}
module Distribution.Client.Compat.Exception (
  mask,
  mask_
  ) where

-- We can't move these functions to Distribution.Compat.Exception because the
-- usage of the MIN_VERSION_base macro breaks bootstrapping.

#if MIN_VERSION_base(4,3,0)
-- it's much less of a headache if we re-export the "real" mask and mask_
-- so there's never more than one definition to conflict
import Control.Exception (mask, mask_)
#else
import Control.Exception (block, unblock)
#endif

#if !MIN_VERSION_base(4,3,0)
-- note: less polymorphic than 'real' mask, to avoid RankNTypes
-- we don't need the full generality where we use it
mask :: ((IO a -> IO a) -> IO b) -> IO b
mask handler = block (handler unblock)

mask_ :: IO a -> IO a
mask_ = block
#endif
