module Distribution.Types.AnnotatedGenericPackageDescription
  (
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Map (Map)
import qualified Data.Map as Map

import Distribution.Types.GenericPackageDescription
import Distribution.Types.Annotation

type SectionArg = String

-- TODO: namespace
data Namespace
  = Field String
  | Section String [SectionArg] Namespace

data AnnotatedGenericPackageDescription = AnnotatedGenericPackageDescription
  { unannotateGenericPackageDescription :: GenericPackageDescription
  , annotationMap :: Map Namespace FieldToAnnotation
  }
