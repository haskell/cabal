module Distribution.Types.AnnotationNamespace where

-- import Distribution.Types.PackageName

data Namespace
  = Field String
  | Section String String {- SectionArgs -} Namespace
  deriving (Eq, Ord, Show)
