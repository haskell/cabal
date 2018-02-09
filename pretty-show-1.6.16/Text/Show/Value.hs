--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Value
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  MIT
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
-- Portability :  Haskell 98
--
-- Generic representation of Showable values.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
module Text.Show.Value ( Name, Value(..) ) where

-- | A name.
type Name     = String

-- | Generic Haskell values.
-- 'NaN' and 'Infinity' are represented as constructors.
-- The 'String' in the literals is the text for the literals \"as is\".
--
-- A chain of infix constructors means that they appeared in the input string
-- without parentheses, i.e
--
-- @1 :+: 2 :*: 3@ is represented with @InfixCons 1 [(":+:",2),(":*:",3)]@, whereas
--
-- @1 :+: (2 :*: 3)@ is represented with @InfixCons 1 [(":+:",InfixCons 2 [(":*:",3)])]@.
data Value    = Con Name [Value]               -- ^ Data constructor
              | InfixCons Value [(Name,Value)] -- ^ Infix data constructor chain
              | Rec Name [ (Name,Value) ]      -- ^ Record value
              | Tuple [Value]                  -- ^ Tuple
              | List [Value]                   -- ^ List
              | Neg Value                      -- ^ Negated value
              | Ratio Value Value              -- ^ Rational
              | Integer String                 -- ^ Non-negative integer
              | Float String                   -- ^ Non-negative floating num.
              | Char String                    -- ^ Character
              | String String                  -- ^ String
                deriving (Eq,Show)
