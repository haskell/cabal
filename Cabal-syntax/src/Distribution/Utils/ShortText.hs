{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Compact representation of short 'Strings'
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

import Distribution.Utils.Structured (Structured (..), nominalStructure)

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import qualified Data.ByteString.Short as BS.Short

-- | Construct 'ShortText' from 'String'
toShortText :: String -> ShortText

-- | Convert 'ShortText' to 'String'
fromShortText :: ShortText -> String

-- | Convert from UTF-8 encoded strict 'ByteString'.
--
-- @since 3.2.0.0
unsafeFromUTF8BS :: BS.ByteString -> ShortText

-- | Text whether 'ShortText' is empty.
--
-- @since 3.2.0.0
null :: ShortText -> Bool

-- | Compact representation of short 'Strings'
--
-- The data is stored internally as UTF8 in an
-- 'BS.Short.ShortByteString'.
--
-- Note: This type is for internal uses (such as e.g. 'PackageName')
-- and shall not be exposed in Cabal's API
--
-- @since 2.0.0.2
newtype ShortText = ST {unST :: BS.Short.ShortByteString}
  deriving (Eq, Ord, Generic, Data)

instance Binary ShortText where
  put = put . unST
  get = fmap ST get

toShortText = ST . BS.Short.toShort . T.encodeUtf8 . T.pack

fromShortText = T.unpack . T.decodeUtf8With T.lenientDecode . BS.Short.fromShort . unST

unsafeFromUTF8BS = ST . BS.Short.toShort

null = BS.Short.null . unST

instance Structured ShortText where structure = nominalStructure

instance NFData ShortText where
  rnf = rnf . unST

instance Show ShortText where
  show = show . fromShortText

instance Read ShortText where
  readsPrec p = map (first toShortText) . readsPrec p

instance Semigroup ShortText where
  ST a <> ST b = ST (mappend a b)

instance Monoid ShortText where
  mempty = ST mempty
  mappend = (<>)

instance IsString ShortText where
  fromString = toShortText

-- | /O(n)/. Length in characters. /Slow/ as converts to string.
--
-- @since 3.2.0.0
length :: ShortText -> Int
length = List.length . fromShortText

-- Note: avoid using it, we use it @cabal check@ implementation, where it's ok.
