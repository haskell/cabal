{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Demo (main) where

#include "MachDeps.h"

import Data.Bits
import GHC.Exts
import Numeric (showHex)

foreign import prim "aToMyWordzh" aToWord# :: Any -> Word#

tAG_MASK :: Int
tAG_MASK = (1 `shift` TAG_BITS) - 1

data Box = Box Any

instance Show Box where
   showsPrec _ (Box a) rs =
    -- unsafePerformIO (print "↓" >> pClosure a) `seq`
    pad_out (showHex addr "") ++ (if tag>0 then "/" ++ show tag else "") ++ rs
     where
       ptr  = W# (aToWord# a)
       tag  = ptr .&. fromIntegral tAG_MASK -- ((1 `shiftL` TAG_BITS) -1)
       addr = ptr - tag
       pad_out ls = '0':'x':ls

asBox :: a -> Box
asBox x = Box (unsafeCoerce# x)

main :: IO ()
main = do
    let box = asBox "foobar"
    putStrLn $ "In Box we have " ++ show box
