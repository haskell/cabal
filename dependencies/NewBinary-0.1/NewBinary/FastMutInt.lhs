{-# OPTIONS -cpp #-}
--
-- (c) The University of Glasgow 2002
--
-- Unboxed mutable Ints

\begin{code}
module NewBinary.FastMutInt(
	FastMutInt, newFastMutInt,
	readFastMutInt, writeFastMutInt,
	incFastMutInt, incFastMutIntBy,
        orFastMutInt
  ) where

#include "MachDeps.h"

#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif


import GHC.Exts
#if __GLASGOW_HASKELL__ >= 504
import GHC.IOBase
import Data.Word
import Data.Bits
#else
import PrelIOBase
import Word
import Bits
#endif

#if __GLASGOW_HASKELL__ < 411
newByteArray# = newCharArray#
#endif
\end{code}

\begin{code}
#ifdef __GLASGOW_HASKELL__
data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt :: IO FastMutInt
newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where I# size = SIZEOF_HSINT

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }

incFastMutInt :: FastMutInt -> IO Int	-- Returns original value
incFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  case writeIntArray# arr 0# (i +# 1#) s of { s ->
  (# s, I# i #) } }

incFastMutIntBy :: FastMutInt -> Int -> IO Int	-- Returns original value
incFastMutIntBy (FastMutInt arr) (I# n) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  case writeIntArray# arr 0# (i +# n) s of { s ->
  (# s, I# i #) } }


-- we should optimize this: ask SimonM :)
orFastMutInt :: FastMutInt -> Word8 -> IO ()
orFastMutInt fmi w = do
  i <- readFastMutInt fmi
  writeFastMutInt fmi (i .|. (fromIntegral w))


\end{code}
#endif

