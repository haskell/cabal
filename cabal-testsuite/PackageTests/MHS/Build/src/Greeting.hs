module Greeting (greeting) where

import Greeting.Internal (compilerName)

greeting :: String
greeting = "Hello from " ++ compilerName
