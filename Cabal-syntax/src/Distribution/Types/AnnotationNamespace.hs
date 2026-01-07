module Distribution.Types.AnnotationNamespace where

import qualified Data.ByteString as BS

-- TODO: import all the types that we need to use as key to index the trivia
import Distribution.Types.VersionRange
import Distribution.Types.Version.Internal
import Distribution.Types.PackageName.Internal
import Distribution.Types.Dependency.Internal
import Distribution.Types.LibraryName.Internal

data Namespace
  = NSVersion Version
  | NSVersionRange VersionRange (Maybe Namespace)
  | NSPackageName PackageName (Maybe Namespace)
  | NSDependency Dependency (Maybe Namespace)
  | NSField BS.ByteString Namespace
  | NSLibrarySection LibraryName Namespace
  deriving (Eq, Ord, Show)
