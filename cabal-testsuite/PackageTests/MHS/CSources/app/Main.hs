module Main (main) where

import Add (add)
import Foreign.C.Types (CInt (..))
import Twice (twice)

foreign import ccall "offset.h csources_offset" offset :: IO CInt

main :: IO ()
main = do
  putStrLn ("add 2 3 = " ++ show (add 2 3))
  putStrLn ("twice 21 = " ++ show (twice 21))
  o <- offset
  putStrLn ("offset = " ++ show o)
