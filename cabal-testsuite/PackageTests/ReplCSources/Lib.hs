module Lib where

foreign import ccall "foo" foo :: Int

bar = foo
