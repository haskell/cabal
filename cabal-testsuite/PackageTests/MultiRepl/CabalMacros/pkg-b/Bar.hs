{-# LANGUAGE CPP #-}
module Bar (foo, bar) where

import Foo (foo)

#if MIN_VERSION_pkg_a(0,1,0)
bar :: Int
bar = 0xdeadc0de
#else
bar :: Int
bar = 0xdeadc0d1
#endif
