module MyLib (greeting) where

import Sub (subGreeting)

greeting :: String
greeting = "hello from relocatable lib; " ++ subGreeting
