module Distribution.Types.AnnotatedGenericPackageDescription
  (
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Map (Map)
import qualified Data.Map as Map

import Distribution.Types.GenericPackageDescription
import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

type SectionArg = String

data AnnotatedGenericPackageDescription = AnnotatedGenericPackageDescription
  { unannotateGenericPackageDescription :: GenericPackageDescription
  , annotationMap :: Map Namespace [Trivium]
  }

