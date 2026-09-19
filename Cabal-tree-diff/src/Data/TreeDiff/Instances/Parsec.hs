{-# OPTIONS_GHC -Wno-orphans #-}
module Data.TreeDiff.Instances.Parsec () where

import Data.TreeDiff
import qualified Text.Parsec as P

instance ToExpr P.ParseError where toExpr = defaultExprViaShow
