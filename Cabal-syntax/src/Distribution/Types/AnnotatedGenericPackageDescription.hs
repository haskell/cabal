{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains 'AnnotatedGenericPackageDescription', useful for exact print
-- We split this from 'GenericPackageDescription' type notably because the
-- exact comments breaks its 'Eq' instance.
module Distribution.Types.AnnotatedGenericPackageDescription
  ( AnnotatedGenericPackageDescription (..)
  , ExactComments
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.ByteString
import Distribution.Parsec.Position (Position)
import Distribution.Types.GenericPackageDescription

data AnnotatedGenericPackageDescription = AnnotatedGenericPackageDescription
  { exactComments :: ExactComments Position
  , unannotatedGpd :: GenericPackageDescription
  }
  deriving (Show, Data, Generic)

type ExactComments ann = Map ann ByteString
