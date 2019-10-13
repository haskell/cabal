module Foo where

-- https://isocpp.org/wiki/faq/mixing-c-and-cpp
compiler :: String
#ifdef __cplusplus
compiler = "Is not C, is C++"
#else
compiler = "Is C, is not C++"
#endif
