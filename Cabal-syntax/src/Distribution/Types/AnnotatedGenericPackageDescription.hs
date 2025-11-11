-- | Contains 'AnnotatedGenericPackageDescription', useful for exact print
-- We split this from 'GenericPackageDescription' type notably because the
-- exact comments breaks its 'Eq' instance.
module Distribution.Types.AnnotatedGenericPackageDescription where


import Distribution.Types.GenericPackageDescription

data AnnotatedGenericPackageDescription = AnnotatedGenericPackageDescription
  { unannotateGpd :: GenericPackageDescription
  , exactComments :: ExactComments Position
  }
  deriving (Show, Data, Generic)

type ExactComments ann = Map ann ByteString
