module Lib (foo) where

import Dep (bar)
import Dep2 (baz)

foo :: Int
foo = bar + baz
