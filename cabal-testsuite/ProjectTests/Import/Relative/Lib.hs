module Lib (foo) where

import Dep1 (bar)
import Dep2 (baz)

foo :: Int
foo = bar + baz
