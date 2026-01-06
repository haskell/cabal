module Distribution.Types.AnnotationNamespace where

import qualified Data.ByteString as BS

-- TODO: import all the types that we need to use as key to index the trivia
import Distribution.Types.PackageName
import Distribution.Types.VersionRange
import Distribution.Types.Version
import Distribution.Types.Dependency
import Distribution.Types.LibraryName

data Namespace
  = NSVersion Version
  | NSVersionRange VersionRange (Maybe Namespace)
  | NSPackageName PackageName (Maybe Namespace)
  | NSDependency Dependency (Maybe Namespace)
  | NSField BS.ByteString Namespace
  | NSLibrarySection LibraryName Namespace
  deriving (Eq, Ord, Show)
