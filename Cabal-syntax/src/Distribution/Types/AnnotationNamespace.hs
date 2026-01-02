module Distribution.Types.AnnotationNamespace where

-- TODO: import all the types that we need to use as key to index the trivia
import Distribution.Types.PackageName

data Namespace
  = Field String
  | Section String String {- SectionArgs -} Namespace
  deriving (Eq, Ord, Show)
