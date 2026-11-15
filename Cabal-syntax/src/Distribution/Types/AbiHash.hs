module Distribution.Types.AbiHash
  ( AbiHash
  , unAbiHash
  , mkAbiHash
  , mkAbiHashText
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec
import Distribution.Pretty

import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint (text)

-- | ABI Hashes
--
-- Use 'mkAbiHash' and 'unAbiHash' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype AbiHash = AbiHash Text
  deriving (Eq, Show, Read, Generic)

-- | Convert 'AbiHash' to 'Text'
--
-- @since 2.0.0.2
unAbiHash :: AbiHash -> Text
unAbiHash (AbiHash h) = h

-- | Construct a 'AbiHash' from a 'String'
--
-- 'mkAbiHash' is the inverse to 'unAbiHash'
--
-- Note: No validations are performed to ensure that the resulting
-- 'AbiHash' is valid
--
-- @since 2.0.0.2
mkAbiHash :: String -> AbiHash
mkAbiHash = AbiHash . Text.pack

mkAbiHashText :: Text -> AbiHash
mkAbiHashText = AbiHash

-- | 'mkAbiHash'
--
-- @since 2.0.0.2
instance IsString AbiHash where
  fromString = mkAbiHash

instance Binary AbiHash
instance Structured AbiHash
instance NFData AbiHash

instance Pretty AbiHash where
  pretty = text . Text.unpack . unAbiHash

instance Parsec AbiHash where
  parsec = fmap mkAbiHash (P.munch isAlphaNum)
