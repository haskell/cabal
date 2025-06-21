module Foo06 (foo06) where

import FooDep01
import FooDep02
import FooDep03
import FooDep04
import FooDep05
import FooDep06
import FooDep07
import FooDep08
import FooDep09
import FooDep10

foo06 :: Int
foo06 = sum
  [ 06
  , fooDep01
  , fooDep02
  , fooDep03
  , fooDep04
  , fooDep05
  , fooDep06
  , fooDep07
  , fooDep08
  , fooDep09
  , fooDep10
  ]
