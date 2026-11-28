module Q (combine) where

import Helper (Helper (..))
import P (Value (..))

-- | Uses the types of both libraries of p.
combine :: Value -> Helper -> Int
combine (Value v) (Helper (Value h)) = v + h
