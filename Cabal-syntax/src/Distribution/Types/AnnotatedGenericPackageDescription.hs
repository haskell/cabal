module Distribution.Types.AnnotatedGenericPackageDescription
  (
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.GenericPackageDescription
import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

data AnnotatedGenericPackageDescription = AnnotatedGenericPackageDescription
  { unannotateGenericPackageDescription :: GenericPackageDescription
  , annotationMap :: Map Namespace [Trivium]
  }

