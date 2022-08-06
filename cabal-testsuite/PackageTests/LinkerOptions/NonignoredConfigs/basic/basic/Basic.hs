module Basic where

funcs :: (a -> b -> c) -> ((a -> b -> c) -> a -> b -> c) -> b -> a -> c
funcs f g = \a b -> (g f) b a

name :: String
name = "Basic"

number :: Integer
number = 8
