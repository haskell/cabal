{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Utils.ShortText
    ( -- * 'ShortText' type
      ShortText
    , toShortText
    , fromShortText

      -- * internal utilities
    , decodeStringUtf8
    , encodeStringUtf8
    ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.String

import Data.String (IsString(..))

#if defined(MIN_VERSION_bytestring)
# if MIN_VERSION_bytestring(0,10,4)
# define HAVE_SHORTBYTESTRING 1
# endif
#endif

#if HAVE_SHORTBYTESTRING
import qualified Data.ByteString.Short as BS.Short
#endif

-- | Construct 'ShortText' from 'String'
toShortText :: String -> ShortText

-- | Convert 'ShortText' to 'String'
fromShortText :: ShortText -> String

-- | Compact representation of short 'Strings'
--
-- The data is stored internally as UTF8 in an
-- 'BS.Short.ShortByteString' when compiled against @bytestring >=
-- 0.10.4@, and otherwise the fallback is to use plain old non-compat
-- '[Char]'.
--
-- Note: This type is for internal uses (such as e.g. 'PackageName')
-- and shall not be exposed in Cabal's API
--
-- @since 2.0.0
#if HAVE_SHORTBYTESTRING
newtype ShortText = ST { unST :: BS.Short.ShortByteString }
                  deriving (Eq,Ord,Generic,Data,Typeable)

# if MIN_VERSION_binary(0,8,1)
instance Binary ShortText where
    put = put . unST
    get = fmap ST get
# else
instance Binary ShortText where
    put = put . BS.Short.fromShort . unST
    get = fmap (ST . BS.Short.toShort) get
# endif

toShortText = ST . BS.Short.pack . encodeStringUtf8

fromShortText = decodeStringUtf8 . BS.Short.unpack . unST
#else
newtype ShortText = ST { unST :: String }
                  deriving (Eq,Ord,Generic,Data,Typeable)

instance Binary ShortText where
    put = put . encodeStringUtf8 . unST
    get = fmap (ST . decodeStringUtf8) get

toShortText = ST

fromShortText = unST
#endif

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
