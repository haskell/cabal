module Greeting.Internal (compilerName) where

import qualified Data.ByteString.Char8 as B

compilerName :: String
compilerName = B.unpack (B.pack "MicroHs")
