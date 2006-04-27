{-# OPTIONS -cpp #-}
--
-- (c) The University of Glasgow 2002
--
-- Binary I/O library, with special tweaks for GHC
--
-- Based on the nhc98 Binary library, which is copyright
-- (c) Malcolm Wallace and Colin Runciman, University of York, 1998.
-- Under the terms of the license for that software, we must tell you
-- where you can obtain the original version of the Binary library, namely
--     http://www.cs.york.ac.uk/fp/nhc98/

module NewBinary.Binary
  ( {-type-}  Bin,
    {-class-} Binary(..),
    {-type-}  BinHandle(..),

   openBinIO, 
   openBinIO_,
   openBinMem,
--   closeBin,

--   getUserData,

   seekBin,
   tellBin,
   tellBinByte,
   castBin,

   writeBinMem,
   readBinMem,

   isEOFBin,

   -- for writing instances:
   putByte,
   getByte,

   -- bit stuff
   putBits,
   getBits,
   flushByte,
   finishByte,
   putMaybeInt,
   getMaybeInt,

   -- lazy Bin I/O
   lazyGet,
   lazyPut,

   -- GHC only:
   ByteArray(..),
   getByteArray,
   putByteArray,

--   getBinFileWithDict,	-- :: Binary a => FilePath -> IO a
--   putBinFileWithDict,	-- :: Binary a => FilePath -> Module -> a -> IO ()

  ) where

#include "MachDeps.h"

import NewBinary.FastMutInt

#if __GLASGOW_HASKELL__ > 503
import GHC.Exts
import GHC.IOBase
import GHC.Real
import Data.Array.IO 		( IOUArray )
import Data.Bits
import Data.Int
import Data.Word
import Data.Char
import Control.Monad
import Control.Exception
import Data.Array
import Data.Array.IO
import Data.Array.Base
import System.IO as IO
import System.IO.Error		( mkIOError, eofErrorType )
import GHC.Handle		
import IOExts ( openFileEx, IOModeEx(..) )
#else
import Bits
import Int
import Word
import Char
import Monad
import Exception
import Array
import IOExts
import GlaExts hiding (ByteArray, newByteArray, freezeByteArray)
import PrelIOBase	--	( IOError(..), IOErrorType(..) )
import PrelReal		--	( Ratio(..) )
import PrelIOBase	-- 	( IO(..) )
import MArray (IOUArray)
#endif

#if __GLASGOW_HASKELL__ < 503
type BinArray = MutableByteArray RealWorld Int
newArray_ bounds     = stToIO (newCharArray bounds)
unsafeWrite arr ix e = stToIO (writeWord8Array arr ix e)
unsafeRead  arr ix   = stToIO (readWord8Array arr ix)

hPutArray h arr sz   = hPutBufBA h arr sz
hGetArray h sz       = hGetBufBA h sz

mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> Exception
mkIOError t location maybe_hdl maybe_filename
  = IOException (IOError maybe_hdl t location ""
		         maybe_filename
  		)

eofErrorType = EOF

#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

#ifndef SIZEOF_HSWORD
#define SIZEOF_HSWORD WORD_SIZE_IN_BYTES
#endif

#else
type BinArray = IOUArray Int Word8
#endif

data BinHandle
  = BinMem {		-- binary data stored in an unboxed array
     off_r :: !FastMutInt,		-- the current offset
     sz_r  :: !FastMutInt,		-- size of the array (cached)
     arr_r :: !(IORef BinArray),	-- the array (bounds: (0,size-1))
     bit_off_r :: !FastMutInt,          -- the bit offset (see end of file)
     bit_cache_r :: !FastMutInt           -- the bit cache  (see end of file)
    }
	-- XXX: should really store a "high water mark" for dumping out
	-- the binary data to a file.

  | BinIO {		-- binary data stored in a file
     off_r :: !FastMutInt,		-- the current offset (cached)
     hdl   :: !IO.Handle,               -- the file handle (must be seekable)
     bit_off_r :: !FastMutInt,          -- the bit offset (see end of file)
     bit_cache_r :: !FastMutInt           -- the bit cache  (see end of file)
   }
	-- cache the file ptr in BinIO; using hTell is too expensive
	-- to call repeatedly.  If anyone else is modifying this Handle
	-- at the same time, we'll be screwed.

data Bin a = BinPtr !Int !Int -- byte/bit
  deriving (Eq, Ord, Show, Bounded)

castBin :: Bin a -> Bin b
castBin (BinPtr i j) = BinPtr i j

class Binary a where
    put_   :: BinHandle -> a -> IO ()
    put    :: BinHandle -> a -> IO (Bin a)
    get    :: BinHandle -> IO a

    -- define one of put_, put.  Use of put_ is recommended because it
    -- is more likely that tail-calls can kick in, and we rarely need the
    -- position return value.
    put_ bh a = do put bh a; return ()
    put bh a  = do p <- tellBin bh; put_ bh a; return p

putAt  :: Binary a => BinHandle -> Bin a -> a -> IO ()
putAt bh p x = do seekBin bh p; put bh x; return ()

getAt  :: Binary a => BinHandle -> Bin a -> IO a
getAt bh p = do seekBin bh p; get bh

openBinIO_ :: IO.Handle -> IO BinHandle
openBinIO_ h = openBinIO h noBinHandleUserData

newZeroInt = do r <- newFastMutInt; writeFastMutInt r 0; return r

--openBinIO :: IO.Handle -> Module -> IO BinHandle
openBinIO h mod = do
  r <- newZeroInt
  o <- newZeroInt
  c <- newZeroInt
--  state <- newWriteState mod
  return (BinIO r h o c)

--openBinMem :: Int -> Module -> IO BinHandle
openBinMem size mod
 | size <= 0 = error "Data.Binary.openBinMem: size must be > 0"   -- fix, was ">= 0"
 | otherwise = do
   arr <- newArray_ (0,size-1)
   arr_r <- newIORef arr
   ix_r <- newFastMutInt
   writeFastMutInt ix_r 0
   sz_r <- newFastMutInt
   writeFastMutInt sz_r size
   o <- newZeroInt
   c <- newZeroInt
--   state <- newWriteState mod
   return (BinMem ix_r sz_r arr_r o c)

noBinHandleUserData = error "Binary.BinHandle: no user data"

--getUserData :: BinHandle -> BinHandleState
--getUserData bh = state bh

tellBin :: BinHandle -> IO (Bin a)
tellBin (BinIO r _ o _)   =  do ix <- readFastMutInt r; bix <- readFastMutInt o; return (BinPtr ix bix)
tellBin (BinMem r _ _ o _) = do ix <- readFastMutInt r; bix <- readFastMutInt o; return (BinPtr ix bix)

tellBinByte (BinIO r _ _ _)    = do ix <- readFastMutInt r; return ix
tellBinByte (BinMem r _ _ _ _) = do ix <- readFastMutInt r; return ix

seekBin :: BinHandle -> Bin a -> IO ()
seekBin bh@(BinIO ix_r h o c) (BinPtr p bit) = do 
  writeFastMutInt ix_r p
  writeFastMutInt o 0
  writeFastMutInt c 0
  hSeek h AbsoluteSeek (fromIntegral p)
  when (bit /= 0) $ getBits bh bit >> return ()
  return ()
seekBin h@(BinMem ix_r sz_r a o c) (BinPtr p bit) = do
  sz <- readFastMutInt sz_r
  if (p >= sz)
	then do expandBin h p
                writeFastMutInt ix_r p
                writeFastMutInt o 0
                writeFastMutInt c 0
                when (bit /= 0) $ getBits h bit >> return ()
                return ()
	else do writeFastMutInt ix_r p
                writeFastMutInt o 0
                writeFastMutInt c 0
                when (bit /= 0) $ getBits h bit >> return ()
                return ()

isEOFBin :: BinHandle -> IO Bool
isEOFBin (BinMem ix_r sz_r a _ _) = do
  ix <- readFastMutInt ix_r
  sz <- readFastMutInt sz_r
  return (ix >= sz)
isEOFBin (BinIO ix_r h _ _) = hIsEOF h

writeBinMem :: BinHandle -> FilePath -> IO ()
writeBinMem (BinIO _ _ _ _) _ = error "Data.Binary.writeBinMem: not a memory handle"
writeBinMem bh@(BinMem ix_r sz_r arr_r bit_off_r bit_cache_r) fn = do
  flushByte bh
  h <- openFileEx fn (BinaryMode WriteMode)
  arr <- readIORef arr_r
  ix  <- readFastMutInt ix_r
  hPutArray h arr ix
  hClose h

flushByte :: BinHandle -> IO ()
flushByte bh = do
  bit_off <- readFastMutInt (bit_off_r bh)
  if bit_off == 0
    then return ()
    else putBits bh (8 - bit_off) 0

finishByte :: BinHandle -> IO ()
finishByte bh = do
  bit_off <- readFastMutInt (bit_off_r bh)
  if bit_off == 0
    then return ()
    else getBits bh (8 - bit_off) >> return ()

readBinMem :: FilePath -> IO BinHandle
readBinMem filename = do
  h <- openFileEx filename (BinaryMode ReadMode)
  filesize' <- hFileSize h
  let filesize = fromIntegral filesize'
  arr <- newArray_ (0,filesize-1)
  count <- hGetArray h arr filesize
  when (count /= filesize)
	(error ("Binary.readBinMem: only read " ++ show count ++ " bytes"))
  hClose h
  arr_r <- newIORef arr
  ix_r <- newFastMutInt
  writeFastMutInt ix_r 0
  sz_r <- newFastMutInt
  writeFastMutInt sz_r filesize
  bit_off_r <- newZeroInt
  bit_cache_r <- newZeroInt
  return (BinMem {-initReadState-} ix_r sz_r arr_r bit_off_r bit_cache_r)

-- expand the size of the array to include a specified offset
expandBin :: BinHandle -> Int -> IO ()
expandBin (BinMem ix_r sz_r arr_r _ _) off = do
   sz <- readFastMutInt sz_r
   let sz' = head (dropWhile (<= off) (iterate (* 2) sz))
   arr <- readIORef arr_r
   arr' <- newArray_ (0,sz'-1)
   sequence_ [ unsafeRead arr i >>= unsafeWrite arr' i
 	     | i <- [ 0 .. sz-1 ] ]
   writeFastMutInt sz_r sz'
   writeIORef arr_r arr'
--   hPutStrLn stderr ("expanding to size: " ++ show sz')
   return ()
expandBin (BinIO _ _ _ _) _ = return ()
	-- no need to expand a file, we'll assume they expand by themselves.

-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 h@(BinMem ix_r sz_r arr_r bit_off_r bit_cache_r) w = do
    bit_off <- readFastMutInt bit_off_r
    if bit_off /= 0 then putBits h 8 w else do   -- only do standard putWord8 if bit_off == 0
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
	-- double the size of the array if it overflows
    if (ix >= sz) 
	then do expandBin h ix
		putWord8 h w
	else do arr <- readIORef arr_r
		unsafeWrite arr ix w
    		writeFastMutInt ix_r (ix+1)
    		return ()
putWord8 bh@(BinIO ix_r h bit_off_r bit_cache_r) w = do
    bit_off <- readFastMutInt bit_off_r
    if bit_off /= 0 then putBits bh 8 w else do
    ix <- readFastMutInt ix_r
    hPutChar h (chr (fromIntegral w))	-- XXX not really correct
    writeFastMutInt ix_r (ix+1)
    return ()

putByteNoBits :: BinHandle -> Word8 -> IO ()
putByteNoBits h@(BinMem ix_r sz_r arr_r _ _) w = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
	-- double the size of the array if it overflows
    if (ix >= sz) 
	then do expandBin h ix
		putByteNoBits h w
	else do arr <- readIORef arr_r
		unsafeWrite arr ix w
    		writeFastMutInt ix_r (ix+1)
    		return ()
putByteNoBits bh@(BinIO ix_r h _ _) w = do
    hPutChar h (chr (fromIntegral w))	-- XXX not really correct
    incFastMutInt ix_r
    return ()

getByteNoBits :: BinHandle -> IO Word8
getByteNoBits h@(BinMem ix_r sz_r arr_r _ _) = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
    when (ix >= sz)  $
	throw (IOException $ mkIOError eofErrorType "Data.Binary.getWord8" Nothing Nothing)
    arr <- readIORef arr_r
    w <- unsafeRead arr ix
    writeFastMutInt ix_r (ix+1)
    return w
getByteNoBits bh@(BinIO ix_r h _ _) = do
    c <- hGetChar h
    incFastMutInt ix_r
    return $! (fromIntegral (ord c))	-- XXX not really correct

getWord8 :: BinHandle -> IO Word8
getWord8 h@(BinMem ix_r sz_r arr_r bit_off_r _) = do
    bit_off <- readFastMutInt bit_off_r
    if bit_off /= 0 then getBits h 8 else do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
    when (ix >= sz)  $
	throw (IOException $ mkIOError eofErrorType "Data.Binary.getWord8" Nothing Nothing)
    arr <- readIORef arr_r
    w <- unsafeRead arr ix
    writeFastMutInt ix_r (ix+1)
    return w
getWord8 bh@(BinIO ix_r h bit_off_r _) = do
    bit_off <- readFastMutInt bit_off_r
    if bit_off /= 0 then getBits bh 8 else do
    ix <- readFastMutInt ix_r
    c <- hGetChar h
    writeFastMutInt ix_r (ix+1)
    return $! (fromIntegral (ord c))	-- XXX not really correct

putByte :: BinHandle -> Word8 -> IO ()
putByte bh w = put_ bh w

getByte :: BinHandle -> IO Word8
getByte = getWord8

-- -----------------------------------------------------------------------------
-- Bit functions

putBits :: BinHandle -> Int -> Word8 -> IO ()
putBits bh num_bits bits {- | num_bits == 0 = return ()
                         | num_bits <  0 = error "putBits cannot write negative numbers of bits"
                         | num_bits >  8 = error "putBits cannot write more than 8 bits at a time"
                         | otherwise    -} = do
  bit_off <- readFastMutInt (bit_off_r bh)
  if num_bits + bit_off < 8
    then do incFastMutIntBy (bit_off_r bh) num_bits
            orFastMutInt (bit_cache_r bh) (bits `shiftL` bit_off)
    else if num_bits + bit_off == 8
           then do writeFastMutInt (bit_off_r bh) 0
                   bit_cache <- {-# SCC "bc1" #-} readFastMutInt (bit_cache_r bh) >>= return . fromIntegral
                   writeFastMutInt (bit_cache_r bh) 0
                   --putByte bh (bit_cache .|. (bits `shiftL` bit_off))    -- won't call putBits because bit_off_r == 0
                   putByteNoBits bh (bit_cache .|. (bits `shiftL` bit_off))

           else do let leftover_bits = 8 - bit_off                       -- we are going over a byte boundary
                   bit_cache <- {-# SCC "bc2" #-} readFastMutInt (bit_cache_r bh) >>= \x -> return ({-# SCC "fi" #-} fromIntegral x)
                   writeFastMutInt (bit_off_r bh) 0
                   writeFastMutInt (bit_cache_r bh) 0
                   {- putByte bh (bit_cache .|. (bits `shiftL` bit_off))  -}  -- won't call putBits
                   putByteNoBits bh (bit_cache .|. (bits `shiftL` bit_off))
                   putBits bh (num_bits - leftover_bits) (bits `shiftR` leftover_bits)

getBits :: BinHandle -> Int -> IO Word8
getBits bh num_bits {- | num_bits == 0 = return 0
                    | num_bits <  0 = error "getBits cannot read negative numbers of bits"
                    | num_bits >  8 = error "getBits cannot read more than 8 bits at a time"
                    | otherwise     -} = do
  bit_off <- readFastMutInt (bit_off_r bh)
  if bit_off == 0
    then do bit_cache <- getByte bh
            if num_bits == 8
              then do writeFastMutInt (bit_off_r   bh) 0
                      writeFastMutInt (bit_cache_r bh) 0
                      return bit_cache
              else do writeFastMutInt (bit_off_r   bh) (fromIntegral num_bits)
                      writeFastMutInt (bit_cache_r bh) (fromIntegral bit_cache)
                      return (bit_cache .&. bit_mask num_bits)
    else if bit_off + num_bits < 8
    then do incFastMutIntBy (bit_off_r bh) num_bits
            bit_cache <- readFastMutInt (bit_cache_r bh) >>= return . fromIntegral
            return ((bit_cache `shiftR` bit_off) .&. bit_mask num_bits)
    else if bit_off + num_bits == 8
    then do writeFastMutInt (bit_off_r bh) 0
            bit_cache <- readFastMutInt (bit_cache_r bh) >>= return . fromIntegral
            writeFastMutInt (bit_cache_r bh) 0
            return ((bit_cache `shiftR` bit_off) .&. bit_mask num_bits)
    else do let leftover_bits = 8 - bit_off
            bit_cache <- readFastMutInt (bit_cache_r bh) >>= return . fromIntegral
            let bits = (bit_cache `shiftR` bit_off) .&. bit_mask leftover_bits
            writeFastMutInt (bit_cache_r bh) 0
            writeFastMutInt (bit_off_r   bh) 0
            {- bit_cache <- getByte bh -}
            -- use a version that doesn't care about bits
            bit_cache <- getByteNoBits bh
            writeFastMutInt (bit_off_r   bh) (num_bits - leftover_bits)
            writeFastMutInt (bit_cache_r bh) (fromIntegral bit_cache)
            return (bits .|. ((bit_cache .&. bit_mask (num_bits - leftover_bits)) `shiftL` leftover_bits))

            
bit_mask n = (complement 0) `shiftR` (8 - n)

-- -----------------------------------------------------------------------------
-- Primitve Word writes

instance Binary Word8 where
  put_ = putWord8
  get  = getWord8

instance Binary Word16 where
  put_ h w = do -- XXX too slow.. inline putWord8?
    putByte h (fromIntegral (w `shiftR` 8))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 8) .|. fromIntegral w2)


instance Binary Word32 where
  put_ h w = do
    putByte h (fromIntegral (w `shiftR` 24))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 24) .|. 
	       (fromIntegral w2 `shiftL` 16) .|. 
	       (fromIntegral w3 `shiftL`  8) .|. 
	       (fromIntegral w4))


instance Binary Word64 where
  put_ h w = do
    putByte h (fromIntegral (w `shiftR` 56))
    putByte h (fromIntegral ((w `shiftR` 48) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 40) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 32) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 24) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR`  8) .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    w5 <- getWord8 h
    w6 <- getWord8 h
    w7 <- getWord8 h
    w8 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 56) .|. 
	       (fromIntegral w2 `shiftL` 48) .|. 
	       (fromIntegral w3 `shiftL` 40) .|. 
	       (fromIntegral w4 `shiftL` 32) .|. 
	       (fromIntegral w5 `shiftL` 24) .|. 
	       (fromIntegral w6 `shiftL` 16) .|. 
	       (fromIntegral w7 `shiftL`  8) .|. 
	       (fromIntegral w8))

-- -----------------------------------------------------------------------------
-- Primitve Int writes

instance Binary Int8 where
  put_ h w = put_ h (fromIntegral w :: Word8)
  get h    = do w <- get h; return $! (fromIntegral (w::Word8))

instance Binary Int16 where
  put_ h w = put_ h (fromIntegral w :: Word16)
  get h    = do w <- get h; return $! (fromIntegral (w::Word16))

instance Binary Int32 where
  put_ h w = put_ h (fromIntegral w :: Word32)
  get h    = do w <- get h; return $! (fromIntegral (w::Word32))

put31ofInt32 :: BinHandle -> Int32 -> IO ()
put31ofInt32 h i = do
    putBits h 7 (fromIntegral (w `shiftR` 24))
    putBits h 8 (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putBits h 8 (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putBits h 8 (fromIntegral (w .&. 0xff))
    where w = fromIntegral i :: Word32

get31ofInt32 :: BinHandle -> IO Int32
get31ofInt32 h = do
    w1 <- getBits  h 7
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 24) .|. 
	       (fromIntegral w2 `shiftL` 16) .|. 
	       (fromIntegral w3 `shiftL`  8) .|. 
	       (fromIntegral w4))

instance Binary Int64 where
  put_ h w = put_ h (fromIntegral w :: Word64)
  get h    = do w <- get h; return $! (fromIntegral (w::Word64))

-- -----------------------------------------------------------------------------
-- Instances for standard types

instance Binary () where
    put_ bh () = return ()
    get  _     = return ()
--    getF bh p  = case getBitsF bh 0 p of (_,b) -> ((),b)

{- updated for bits
instance Binary Bool where
    put_ bh b = putByte bh (fromIntegral (fromEnum b))
    get  bh   = do x <- getWord8 bh; return $! (toEnum (fromIntegral x))
--    getF bh p = case getBitsF bh 1 p of (x,b) -> (toEnum x,b)
-}

instance Binary Bool where
    put_ bh True  = putBits bh 1 1
    put_ bh False = putBits bh 1 0
    get  bh = do b <- getBits bh 1; return (b == 1)

instance Binary Char where
    put_  bh c = put_ bh (fromIntegral (ord c) :: Word32)
    get  bh   = do x <- get bh; return $! (chr (fromIntegral (x :: Word32)))
--    getF bh p = case getBitsF bh 8 p of (x,b) -> (toEnum x,b)

instance Binary Int where
#if SIZEOF_HSINT == 4
    put_ bh i = put_ bh (fromIntegral i :: Int32)
    get  bh = do
	x <- get bh
	return $! (fromIntegral (x :: Int32))
#elif SIZEOF_HSINT == 8
    put_ bh i = put_ bh (fromIntegral i :: Int64)
    get  bh = do
	x <- get bh
	return $! (fromIntegral (x :: Int64))
#else
#error "unsupported sizeof(HsInt)"
#endif
--    getF bh   = getBitsF bh 32

{-
instance Binary a => Binary [a] where
    put_ bh []     = putByte bh 0
    put_ bh (x:xs) = do putByte bh 1; put_ bh x; put_ bh xs
    get bh         = do h <- getWord8 bh
                        case h of
                          0 -> return []
                          _ -> do x  <- get bh
                                  xs <- get bh
                                  return (x:xs)
-}
instance Binary a => Binary [a] where
    put_ bh l =
	do put_ bh (length l)
	   mapM (put_ bh) l
	   return ()
    get bh =
	do len <- get bh
	   mapM (\_ -> get bh) [1..(len::Int)]

instance (Binary a, Binary b) => Binary (a,b) where
    put_ bh (a,b) = do put_ bh a; put_ bh b
    get bh        = do a <- get bh
                       b <- get bh
                       return (a,b)

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put_ bh (a,b,c) = do put_ bh a; put_ bh b; put_ bh c
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         return (a,b,c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put_ bh (a,b,c,d) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         return (a,b,c,d)

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a,b,c,d,e) where
    put_ bh (a,b,c,d,e) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d; put_ bh e
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         e <- get bh
                         return (a,b,c,d,e)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f) => Binary (a,b,c,d,e,f) where
    put_ bh (a,b,c,d,e,f) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d; put_ bh e; put_ bh f
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         e <- get bh
                         f <- get bh
                         return (a,b,c,d,e,f)

instance Binary a => Binary (Maybe a) where
    put_ bh Nothing  = putByte bh 0
    put_ bh (Just a) = do putByte bh 1; put_ bh a
    get bh           = do h <- getWord8 bh
                          case h of
                            0 -> return Nothing
                            _ -> do x <- get bh; return (Just x)

putMaybeInt :: BinHandle -> Maybe Int -> IO ()
getMaybeInt :: BinHandle -> IO (Maybe Int)
putMaybeInt bh Nothing = putBits bh 1 0
putMaybeInt bh (Just i) = do putBits bh 1 1; put31ofInt32 bh (fromIntegral i)

getMaybeInt bh = do 
  b <- getBits bh 1
  case b of
    0 -> return Nothing
    _ -> do i <- get31ofInt32 bh
            return (Just (fromIntegral i))

{- RULES get = getMaybeInt -}

{- SPECIALIZE put_ :: BinHandle -> Maybe Int -> IO () = putMaybeInt -}
{- SPECIALIZE get  :: BinHandle -> IO (Maybe Int)     = getMaybeInt -}


instance (Binary a, Binary b) => Binary (Either a b) where
    put_ bh (Left  a) = do putByte bh 0; put_ bh a
    put_ bh (Right b) = do putByte bh 1; put_ bh b
    get bh            = do h <- getWord8 bh
                           case h of
                             0 -> do a <- get bh ; return (Left a)
                             _ -> do b <- get bh ; return (Right b)

instance Binary Integer where
    put_ bh (S# i#) = do putByte bh 0; put_ bh (I# i#)
    put_ bh (J# s# a#) = do
 	p <- putByte bh 1;
	put_ bh (I# s#)
	let sz# = sizeofByteArray# a#  -- in *bytes*
	put_ bh (I# sz#)  -- in *bytes*
	putByteArray bh a# sz#
   
    get bh = do 
	b <- getByte bh
	case b of
	  0 -> do (I# i#) <- get bh
		  return (S# i#)
	  _ -> do (I# s#) <- get bh
		  sz <- get bh
		  (BA a#) <- getByteArray bh sz
		  return (J# s# a#)

putByteArray :: BinHandle -> ByteArray# -> Int# -> IO ()
putByteArray bh a s# = loop 0#
  where loop n# 
	   | n# ==# s# = return ()
	   | otherwise = do
	   	putByte bh (indexByteArray a n#)
		loop (n# +# 1#)

getByteArray :: BinHandle -> Int -> IO ByteArray
getByteArray bh (I# sz) = do
  (MBA arr) <- newByteArray sz 
  let loop n
	   | n ==# sz = return ()
	   | otherwise = do
		w <- getByte bh 
		writeByteArray arr n w
		loop (n +# 1#)
  loop 0#
  freezeByteArray arr


data ByteArray = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray# sz s of { (# s, arr #) ->
  (# s, MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s, arr #) ->
  (# s, BA arr #) }

writeByteArray :: MutableByteArray# RealWorld -> Int# -> Word8 -> IO ()

writeByteArray arr i w8 = IO $ \s ->
  case fromIntegral w8 of { W# w# -> 
  case writeCharArray# arr i (chr# (word2Int# w#)) s  of { s ->
  (# s , () #) }}

indexByteArray a# n# = fromIntegral (I# (ord# (indexCharArray# a# n#)))

instance (Integral a, Binary a) => Binary (Ratio a) where
    put_ bh (a :% b) = do put_ bh a; put_ bh b
    get bh = do a <- get bh; b <- get bh; return (a :% b)

instance Binary (Bin a) where
  put_ bh (BinPtr i j) = put_ bh (i,j)
  get bh = do (i,j) <- get bh; return (BinPtr i j)

-- -----------------------------------------------------------------------------
-- Lazy reading/writing

lazyPut :: Binary a => BinHandle -> a -> IO ()
lazyPut bh a = do
	-- output the obj with a ptr to skip over it:
    pre_a <- tellBin bh
    put_ bh pre_a	-- save a slot for the ptr
    put_ bh a		-- dump the object
    q <- tellBin bh 	-- q = ptr to after object
    putAt bh pre_a q 	-- fill in slot before a with ptr to q
    seekBin bh q	-- finally carry on writing at q

lazyGet :: Binary a => BinHandle -> IO a
lazyGet bh = do
    p <- get bh		-- a BinPtr
    p_a <- tellBin bh
    a <- unsafeInterleaveIO (getAt bh p_a)
    seekBin bh p -- skip over the object for now
    return a

-- -----------------------------------------------------------------------------
-- BinHandleState
{-
type BinHandleState = 
	(Module, 
	 IORef Int,
	 IORef (UniqFM (Int,FastString)),
	 Array Int FastString)

initReadState :: BinHandleState
initReadState = (undef, undef, undef, undef)

newWriteState :: Module -> IO BinHandleState
newWriteState m = do
  j_r <- newIORef 0
  out_r <- newIORef emptyUFM
  return (m,j_r,out_r,undef)

undef = error "Binary.BinHandleState"

-- -----------------------------------------------------------------------------
-- FastString binary interface

getBinFileWithDict :: Binary a => FilePath -> IO a
getBinFileWithDict file_path = do
  bh <- Binary.readBinMem file_path
  magic <- get bh
  when (magic /= binaryInterfaceMagic) $
	throwDyn (ProgramError (
	   "magic number mismatch: old/corrupt interface file?"))
  dict_p <- Binary.get bh		-- get the dictionary ptr
  data_p <- tellBin bh
  seekBin bh dict_p
  dict <- getDictionary bh
  seekBin bh data_p
  let (mod, j_r, out_r, _) = state bh
  get bh{ state = (mod,j_r,out_r,dict) }

initBinMemSize = (1024*1024) :: Int

binaryInterfaceMagic = 0x1face :: Word32

putBinFileWithDict :: Binary a => FilePath -> Module -> a -> IO ()
putBinFileWithDict file_path mod a = do
  bh <- openBinMem initBinMemSize mod
  put_ bh binaryInterfaceMagic
  p <- tellBin bh
  put_ bh p		-- placeholder for ptr to dictionary
  put_ bh a
  let (_, j_r, fm_r, _) = state bh
  j <- readIORef j_r
  fm <- readIORef fm_r
  dict_p <- tellBin bh
  putAt bh p dict_p	-- fill in the placeholder
  seekBin bh dict_p	-- seek back to the end of the file
  putDictionary bh j (constructDictionary j fm)
  writeBinMem bh file_path
  
type Dictionary = Array Int FastString
	-- should be 0-indexed

putDictionary :: BinHandle -> Int -> Dictionary -> IO ()
putDictionary bh sz dict = do
  put_ bh sz
  mapM_ (putFS bh) (elems dict)

getDictionary :: BinHandle -> IO Dictionary
getDictionary bh = do 
  sz <- get bh
  elems <- sequence (take sz (repeat (getFS bh)))
  return (listArray (0,sz-1) elems)

constructDictionary :: Int -> UniqFM (Int,FastString) -> Dictionary
constructDictionary j fm = array (0,j-1) (eltsUFM fm)

putFS bh (FastString id l ba) = do
  put_ bh (I# l)
  putByteArray bh ba l
putFS bh s = error ("Binary.put_(FastString): " ++ unpackFS s)
	-- Note: the length of the FastString is *not* the same as
	-- the size of the ByteArray: the latter is rounded up to a
	-- multiple of the word size.
  
{- -- possible faster version, not quite there yet:
getFS bh@BinMem{} = do
  (I# l) <- get bh
  arr <- readIORef (arr_r bh)
  off <- readFastMutInt (off_r bh)
  return $! (mkFastSubStringBA# arr off l)
-}
getFS bh = do
  (I# l) <- get bh
  (BA ba) <- getByteArray bh (I# l)
  return $! (mkFastSubStringBA# ba 0# l)

instance Binary FastString where
  put_ bh f@(FastString id l ba) =
    case getUserData bh of { (_, j_r, out_r, dict) -> do
    out <- readIORef out_r
    let uniq = getUnique f
    case lookupUFM out uniq of
	Just (j,f)  -> put_ bh j
	Nothing -> do
	   j <- readIORef j_r
	   put_ bh j
	   writeIORef j_r (j+1)
	   writeIORef out_r (addToUFM out uniq (j,f))
    }
  put_ bh s = error ("Binary.put_(FastString): " ++ show (unpackFS s))

  get bh = do 
	j <- get bh
	case getUserData bh of (_, _, _, arr) -> return (arr ! j)
-}



{----------------------------------------------------------------------
 ---------- Hal's Notes -----------------------------------------------
 ----------------------------------------------------------------------

We are adding support for 

  putBits   :: BinHandle -> Int -> Word8 -> IO ()
  getBits   :: BinHandle -> Int -> IO Word8
  flushBits :: BinHandle -> Int -> IO ()
  closeHandle :: BinHandle -> IO ()

where

  `putBits bh num_bits bits' writes the right-most num_bits of bits to
  bh.  `getBits bh num_bits` reads num_bits from bh and stores them in
  the right-most positions of the result.  flushBits bh n alignes the
  stream to the next 2^n bit boundary.  closeHandle flushes all
  remaining bits and closes the handle.

In order to implement this, we need to extend the BinHandles with two
fields: bit_off_r :: Int and bit_cache :: Word8.  Based on this, the
basic implementations look something like this:

putBits bh num_bits bits =
  if num_bits + bit_off_r <= 8
    then bit_off_r += num_bits
         add num_bits of bits to the tail of bit_cache
         if bit_off_r == 8
           then write bit_cache and set bit_cache = 0, bit_off_r = 0
    else let leftover_bits = 8 - bit_off_r
         add leftover_bits of bits to tail of bit_cache
         write bit_cache and set bit_cache = 0, bit_off_r = 0
         putBits bh (num_bits - leftover_bits) (bits >> leftover_bits)

(note that this will recurse at most once)

getBits bh num_bits =
  if bit_off_r == 0
    then bit_cache <- read a byte
         bit_off_r = num_bits
         if bit_off_r == 8, set bit_off_r = 0, bit_cache = 0
    else if bit_off_r + num_bits <= 8
           then bit_off_r += num_bits
                bits = bits from bit_off_r -> bit_off_r+num_bits of bit_cache
                if bit_off_r == 8, set bit_off_r = 0, bit_cache = 0
                return bits
           else let leftover_bits = 8 - bit_off_r
                bits = (last leftover_bits from bit_cache) << (num_bits - leftover_bits)
                bit_cache <- read a byte
                bit_off_r = num_bits - leftover_bits
                return (bits || first (num_bits - leftover_bits) of bit_cache)

Now, we must also modify putByte/getByte.  In these, we do a quick
check to see if bit_off_r == 0; if it does, then we just execute
normally.  Otherwise, we just call putBits/getBits with num_bits=8.

closeHandle bh =
  if bit_off_r == 0
    then close the handle
    else write bit_cache and set bit_cache = 0, bit_off_r =0
         close the handle

-}
