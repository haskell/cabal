module Lib (bar) where

import Generated (foo)

bar :: String
bar = "The module says: " ++ foo
