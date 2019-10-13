module Foo where

foo :: Int
#ifdef TEST_OPTION
foo = #{const TEST_OPTION}
#else
foo = -1
#endif
