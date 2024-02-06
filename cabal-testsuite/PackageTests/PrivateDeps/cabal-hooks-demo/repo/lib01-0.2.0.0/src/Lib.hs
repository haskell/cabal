{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module Lib(A(..), inc) where

import Data.Binary
import GHC.Generics

data A = A { value :: Int } | B deriving (Show, Generic)

deriving instance Binary A

inc :: A -> A
inc (A {}) = B
inc B = B

