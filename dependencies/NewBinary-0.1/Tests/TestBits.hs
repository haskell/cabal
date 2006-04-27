module TestBits
    where

import Binary
import Data.Bits
import Data.Word
import System.IO
import Data.IORef
import IOExts

helpCast :: IORef a
helpCast = unsafePerformIO (newIORef undefined)  -- I promise I'll only use this safely ;)

cast :: a -> b
cast i = unsafePerformIO $ do
  writeIORef helpCast i
  readIORef helpCast

data BinItem = ByteItem Word8
             | BitItem  Int Word8   -- num_bits bits
             | FlushItem            -- flush byte
             | forall a . (Eq a, Binary a, Show a) => BinaryItem a  -- some arbitrary data
             | forall a . (Eq a, Show a) => PutItem a (BinHandle -> a -> IO ()) (BinHandle -> IO a)

instance Eq BinItem where
    ByteItem w == ByteItem v = w == v
    BitItem n w == BitItem m v = n == m && w == v
    FlushItem == FlushItem = True
    BinaryItem a == BinaryItem b = cast a == b
    PutItem a _ _ == PutItem b _ _ = cast a == b

instance Show BinItem where
    showsPrec i (ByteItem w) = showsPrec i w
    showsPrec i (BitItem n w) = showsBits n
        where showsBits 0 = showsBit (w .&. 1)
              showsBits n = showsBit ((w `shiftR` n) .&. 1) . showsBits (n-1)
              showsBit 0 = showChar '0'
              showsBit 1 = showChar '1'
    showsPrec i FlushItem = showChar 'F'
    showsPrec i (BinaryItem a) = showChar '<' . showsPrec i a . showChar '>'
    showsPrec i (PutItem a _ _) = showChar '{' . showsPrec i a . showChar '}'

mkTestMem :: [BinItem] -> IO [BinItem]   -- should be identity
mkTestMem bil = do
  bin <- openBinMem 1 undefined
  pos <- tellBin bin
  mapM_ (writeItem bin) bil
  seekBin bin pos
  mapM (readItem bin) bil

mkTestIO :: [BinItem] -> IO [BinItem]
mkTestIO bil = do
  h <- openFile "tmp" WriteMode
  bin <- openBinIO_ h
  mapM_ (writeItem bin) bil
  hClose h

  h <- openFile "tmp" ReadMode
  bin <- openBinIO_ h
  r <- mapM (readItem bin) bil
  hClose h

  return r


writeItem bin (ByteItem  w)  = putByte bin w
writeItem bin (BitItem n w)  = putBits bin n w
writeItem bin FlushItem      = flushByte bin
writeItem bin (BinaryItem a) = put_ bin a
writeItem bin (PutItem a p _)  = p bin a

readItem bin (ByteItem  _)  = getByte    bin   >>= return . ByteItem
readItem bin (BitItem n _)  = getBits    bin n >>= return . BitItem n
readItem bin FlushItem      = finishByte bin   >>  return   FlushItem
readItem bin (BinaryItem (_::a)) = (get        bin :: IO a)   >>= return . BinaryItem
readItem bin (PutItem _ p g) = do a <- g bin; return (PutItem a p g)

runTestWith mkTest test = do
  res <- mkTest test
  if res == test
    then putStrLn "Passed!"
    else putStrLn ("Failed: " ++ show test ++ " " ++ show res)

runTestMem = runTestWith mkTestMem
runTestIO  = runTestWith mkTestIO

-- ----------------------------------------
-- Tests
-- ----------------------------------------

byteTest = map ByteItem [1,2,3,4,5]

bitTest1 = [BitItem 3 6, FlushItem]
bitTest2 = [BitItem 3 6, BitItem 4 9, BitItem 1 0, FlushItem]
bitTest3 = [BitItem 6 10, BitItem 6 10, FlushItem]

flushTest1 = [BitItem 3 6, FlushItem, BitItem 4 9, BitItem 1 0, FlushItem]
flushTest2 = [ByteItem 1, FlushItem, FlushItem, FlushItem, FlushItem, ByteItem 2, FlushItem]

comboTest1 = [ByteItem 5, BitItem 3 6, FlushItem, ByteItem 9, BitItem 4 9, BitItem 1 0, FlushItem, ByteItem 84, BitItem 3 2, FlushItem]
comboTest2 = [ByteItem 5, BitItem 3 6, ByteItem 9, BitItem 7 9, BitItem 4 0, ByteItem 84, BitItem 3 2, FlushItem]

maybeTest1 = [BinaryItem (Just 5::Maybe Int), BinaryItem (Nothing::Maybe Int), BinaryItem (Just 0::Maybe Int), BinaryItem (Just 1::Maybe Int), BinaryItem (Nothing :: Maybe Int), FlushItem]
maybeTest2 = map (\i -> PutItem i putMaybeInt getMaybeInt) [Just 5, Nothing, Just 0, Just 1, Nothing] ++ [FlushItem]