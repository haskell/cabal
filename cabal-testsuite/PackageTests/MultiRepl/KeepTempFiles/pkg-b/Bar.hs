module Bar (foo, bar) where

import Foo (foo)

bar :: String
bar = "foo is " <> show foo
