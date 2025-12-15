module Distribution.Types.Annotation
  ( FieldToAnnotation
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Map (Map)
import qualified Data.Map as Map

import Distribution.Types.Dependency

type FieldToAnnotation = Map FieldData [Trivium]

data FieldData
  = BuildDepends Dependency
  deriving (Eq, Ord)

data Trivium
  = FieldNth Int
  | Nth Int
  | PreTrivia String
  | PostTrivia String
