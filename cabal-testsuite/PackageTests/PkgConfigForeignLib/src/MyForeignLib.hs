{-# LANGUAGE ForeignFunctionInterface #-}

module MyForeignLib (mylibValue) where

foreign import ccall "mylib_value" mylibValue :: IO Int
