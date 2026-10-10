module Greet (greet) where

import MyLib (myLib)

greet :: String
#ifdef LOUD
greet = myLib ++ "!!!"
#else
greet = myLib
#endif
