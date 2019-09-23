{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

data V
data Vector = Vector (Ptr V)

foreign import ccall "newVector" c_newVector :: Int -> IO (Ptr V)
foreign import ccall "getValueAt" c_getValueAt :: (Ptr V) -> Int -> Int
foreign import ccall "setValueAt" c_setValueAt :: (Ptr V) -> Int -> Int -> IO ()
foreign import ccall "pushBack" c_pushBack :: (Ptr V) -> Int -> IO ()
foreign import ccall "erase" c_erase :: (Ptr V) -> Int -> Int -> IO () -- vec, first, last
foreign import ccall "insert" c_insert :: (Ptr V) -> Int -> Int -> Int -> IO () -- vec, first, last, value
foreign import ccall "clear" c_clear :: (Ptr V) -> Int
foreign import ccall "length" c_length :: (Ptr V) -> Int

toListHelper :: (Ptr V) -> Int -> Int -> [Int] -> [Int]
toListHelper vect n len xs = if n >= 0 then [c_getValueAt vect n] ++ toListHelper vect (n - 1) len xs  else []

toList :: (Ptr V) -> [Int]
toList vect = let len = (c_length vect) in toListHelper vect (len - 1) len []

foreign import ccall "newIntDict" c_newIntDict :: IO (Ptr V)
foreign import ccall "add" c_add :: (Ptr V) -> Int -> Int -> IO ()
foreign import ccall "intDictRemove" c_intDictRemove :: (Ptr V) -> Int -> IO ()
foreign import ccall "lookup" c_lookup :: (Ptr V) -> Int -> Int
foreign import ccall "find" c_find :: (Ptr V) -> Int -> Int
foreign import ccall "size" c_size :: (Ptr V) -> Int

mylookup :: (Ptr V) -> Int -> Maybe Int
mylookup d k = if (c_find d k == 0) then Nothing else Just (c_lookup d k)

updateForN :: (Ptr V) -> Int -> Int -> Int -> IO ()
updateForN indicators n current n_max = if current > n_max then return () else do
  c_setValueAt indicators current 1
  updateForN indicators n (current + n) n_max

updateForAll :: (Ptr V) -> Int -> Int -> IO ()
updateForAll indicators n n_max = if n >= n_max then return () else (if (c_getValueAt indicators n) == 1 then updateForAll indicators (n + 1) n_max else do
  updateForN indicators n (n * 2) n_max
  updateForAll indicators (n + 1) n_max)

setMain :: IO ()
setMain = do
  s <- c_newIntDict
  print $ c_size s
  c_add s 3 5
  {-print $ mylookup s 3-}
  c_add s 3 3
  print $ mylookup s 3
  c_add s 5 5
  print $ mylookup s 3
  print $ mylookup s 5
  {-c_add s 5 5-}
  print $ c_size s
  c_add s 6 6
  c_intDictRemove s 3
  c_intDictRemove s 5
  c_add s 5 5
  print $ c_size s
  print $ mylookup s 3
  c_add s 3 3
  {-[>c_add s 3 1<]-}
  {-[>print $ c_find s 3<]-}
  {-[>print $ c_find s 5<]-}
  print "Haskell rocks!"
  print $ c_size s
  print $ mylookup s 3
  print $ mylookup s 6

main :: IO ()
main = let n = 10000000 in do
  x <- c_newVector (n + 2)
  updateForAll x 2 n
  c_setValueAt x 3 8
  print $ c_getValueAt x 0
  print $ c_getValueAt x 1
  print $ c_getValueAt x 2
  print $ c_getValueAt x 3
  print $ c_getValueAt x 4
  print $ c_getValueAt x 5
  print $ c_getValueAt x 6
  print $ c_getValueAt x 7
  c_erase x 0 1
  print $ (sum . map (\z -> 1 - z)) (toList x) - 1 
  setMain
