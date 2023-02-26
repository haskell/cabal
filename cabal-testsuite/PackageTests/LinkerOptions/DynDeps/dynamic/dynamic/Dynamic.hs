module Dynamic where

simple :: (a -> b -> c) -> b -> a -> c
simple f = \a b -> f b a

name :: String
name = "Dynamic"

number :: Integer
number = 3
