module Distribution.Types.AnnotationNamespace where

-- TODO: import all the types that we need to use as key to index the trivia
import Distribution.Types.PackageName
import Distribution.Types.VersionRange
import Distribution.Types.Version
import Distribution.Types.Dependency

data Namespace
  = NSVersion Version
  | NSVersionRange VersionRange
  | NSPackageName PackageName
  | NSDepedency Dependency
  | Section String String {- SectionArgs -} Namespace
  deriving (Eq, Ord, Show)
