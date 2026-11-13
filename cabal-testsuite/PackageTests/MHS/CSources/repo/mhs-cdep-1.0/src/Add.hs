{-# LANGUAGE CApiFFI #-}

module Add (add) where

import Foreign.C.Types (CInt (..))

foreign import capi "add.h cdep_add" add :: CInt -> CInt -> CInt
