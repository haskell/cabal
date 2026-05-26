-- | This is a legacy compatibility layer,
-- new developments can use "Data.Text" directly.
--
-- This module is designed to be import qualified
--
-- @
-- import Distribution.Utils.ShortText (ShortText)
-- import qualified Distribution.Utils.ShortText as ShortText
-- @
module Distribution.Utils.ShortText
  ( -- * 'ShortText' type
    ShortText
  , toShortText
  , fromShortText
  , unsafeFromUTF8BS

    -- * Operations
  , null
  , length
  ) where

import Distribution.Compat.Prelude hiding (length, null)
import Prelude ()

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

-- | Construct 'ShortText' from 'String'
toShortText :: String -> ShortText
toShortText = T.pack

-- | Convert 'ShortText' to 'String'
fromShortText :: ShortText -> String
fromShortText = T.unpack

-- | Convert from UTF-8 encoded strict 'ByteString'.
--
-- @since 3.2.0.0
unsafeFromUTF8BS :: BS.ByteString -> ShortText
unsafeFromUTF8BS = T.decodeUtf8With T.lenientDecode

-- | Text whether 'ShortText' is empty.
--
-- @since 3.2.0.0
null :: ShortText -> Bool
null = T.null

-- | @since 2.0.0.2
type ShortText = T.Text

-- | /O(n)/. Length in characters.
--
-- @since 3.2.0.0
length :: ShortText -> Int
length = T.length
