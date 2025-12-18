module Distribution.Types.AnnotatedGenericPackageDescription
  (
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Map (Map)
import qualified Data.Map as Map

import Distribution.Types.GenericPackageDescription
import Distribution.Types.Dependency

type SectionArg = String

-- TODO: namespace
data Namespace
  = Field String
  | Section String [SectionArg] Namespace

data AnnotatedGenericPackageDescription = AnnotatedGenericPackageDescription
  { unannotateGenericPackageDescription :: GenericPackageDescription
  , annotationMap :: Map Namespace FieldToAnnotation
  }

type FieldToAnnotation = Map FieldData [Trivium]

data FieldData
  = BuildDepends Dependency

data Trivium
  = FieldNth Int
  | Nth Int
  | PreTrivia String
  | PostTrivia String
