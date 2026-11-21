module Distribution.Types.PackageName
  ( PackageName
  , unPackageName
  , mkPackageName
  , unPackageNameST
  , mkPackageNameST
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Text (Text)
import qualified Data.Text as Text
import Distribution.Parsec
import Distribution.Pretty
import qualified Text.PrettyPrint as Disp

-- | A package name.
--
-- Use 'mkPackageName' and 'unPackageName' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype PackageName = PackageName Text
  deriving (Generic, Read, Show, Eq, Ord, Data)

-- | Convert 'PackageName' to 'String'
unPackageName :: PackageName -> String
unPackageName (PackageName s) = Text.unpack s

-- | @since 3.4.0.0
unPackageNameST :: PackageName -> Text
unPackageNameST (PackageName s) = s

-- | Construct a 'PackageName' from a 'Text'
--
-- 'mkPackageName' is the inverse to 'unPackageName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 2.0.0.2
mkPackageName :: String -> PackageName
mkPackageName = PackageName . Text.pack

-- | Construct a 'PackageName' from a 'Text'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 3.4.0.0
mkPackageNameST :: Text -> PackageName
mkPackageNameST = PackageName

-- | 'mkPackageName'
--
-- @since 2.0.0.2
instance IsString PackageName where
  fromString = mkPackageName

instance Binary PackageName
instance Structured PackageName

instance Pretty PackageName where
  pretty = Disp.text . unPackageName

instance Parsec PackageName where
  parsec = mkPackageNameST <$> parsecUnqualComponentName

instance NFData PackageName where
  rnf (PackageName pkg) = rnf pkg
