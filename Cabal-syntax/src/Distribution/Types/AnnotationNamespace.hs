module Distribution.Types.AnnotationNamespace where

data Namespace
  = Field String
  | Section String String {- SectionArgs -} Namespace
  deriving (Eq, Ord)

