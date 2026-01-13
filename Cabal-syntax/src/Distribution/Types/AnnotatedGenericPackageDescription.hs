{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.AnnotatedGenericPackageDescription
  ( AnnotatedGenericPackageDescription (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Control.DeepSeq

import Distribution.Types.GenericPackageDescription
import Distribution.Types.Annotation

data AnnotatedGenericPackageDescription = AnnotatedGenericPackageDescription
  { unAnnotateGPD :: GenericPackageDescription
  , annotationMap :: TriviaTree
  }
  deriving (Generic)

instance NFData AnnotatedGenericPackageDescription
