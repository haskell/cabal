module Distribution.Utils.String
  ( trim
  ) where

import Data.List (dropWhileEnd)
import GHC.Unicode (isSpace)

-- @since 3.8.0.0
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
