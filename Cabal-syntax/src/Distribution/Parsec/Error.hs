{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Parsec.Error
  ( PError (..)
  , PErrorWithSource (..)
  , showPError
  , showPErrorWithSource
  ) where

import Distribution.Compat.Prelude
import Distribution.Parsec.Position
import Distribution.Parsec.Source
import Distribution.Parsec.Warning -- TODO: Move PSource into own module
import System.FilePath (normalise)
import Prelude ()

-- | Parser error.
data PError = PError {perrorPosition :: Position, perrorMessage :: String}
  deriving (Show, Generic)

data PErrorWithSource src = PErrorWithSource {perrorSource :: !(PSource src), perror :: !PError}
  deriving (Show, Generic, Functor)

instance Binary PError
instance NFData PError where rnf = genericRnf

showPError :: FilePath -> PError -> String
showPError fpath (PError pos msg) =
  normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg

showPErrorWithSource :: PErrorWithSource String -> String
showPErrorWithSource (PErrorWithSource source (PError pos msg)) =
  showPError (showPSourceAsFilePath source) (PError pos msg)
