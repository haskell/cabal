{-# LANGUAGE ForeignFunctionInterface #-}
module DepClang (hello) where

import Foreign.C.String

hello :: IO ()
hello = do
  version <- clang_version >>= peekCString
  putStrLn ("Hello from dep-clang (" ++ version ++ ")!")

foreign import ccall "clang_version" clang_version :: IO CString
