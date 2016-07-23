module Distribution.Solver.Modular.Degree where

-- | Approximation of the branching degree.
--
-- This is designed for computing the branching degree of a goal choice
-- node. If the degree is 0 or 1, it is always good to take that goal,
-- because we can either abort immediately, or have no other choice anyway.
--
-- So we do not actually want to compute the full degree (which is
-- somewhat costly) in cases where we have such an easy choice.
--
data Degree = ZeroOrOne | Two | Other
  deriving (Show, Eq)

instance Ord Degree where
  compare ZeroOrOne _         = LT -- lazy approximation
  compare _         ZeroOrOne = GT -- approximation
  compare Two       Two       = EQ
  compare Two       Other     = LT
  compare Other     Two       = GT
  compare Other     Other     = EQ
