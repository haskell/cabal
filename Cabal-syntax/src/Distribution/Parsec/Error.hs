{-# LANGUAGE DeriveGeneric #-}

module Distribution.Parsec.Error
  ( PError (..)
  , showPError
  ) where

import Distribution.Compat.Prelude
import Distribution.Parsec.Position
import System.FilePath (normalise)
import Prelude ()

-- | Parser error.
data PError = PError Position String
  deriving (Show, Generic)

instance Binary PError
instance NFData PError where rnf = genericRnf

showPError :: FilePath -> PError -> String
showPError fpath (PError pos msg) =
  normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg
