module SetupHelper (setupHelperDefaultMain) where

import Distribution.Simple

setupHelperDefaultMain = putStrLn "This is setup-helper-1.0." >> defaultMain
