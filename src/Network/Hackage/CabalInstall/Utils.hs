module Network.Hackage.CabalInstall.Utils where

import Distribution.Compat.ReadP (ReadP, readP_to_S)
import Distribution.Verbosity
import Network.Hackage.CabalInstall.Types

import Data.Char (isSpace)
import Data.Maybe (listToMaybe)


isVerbose cfg = configVerbose cfg >= verbose

readPToMaybe :: ReadP r a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- readP_to_S p str, all isSpace s ]
