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

    -- * internal utilities
  , decodeStringUtf8
  , encodeStringUtf8
  ) where

import Distribution.Compat.Prelude hiding (length, null)
import Prelude ()

import Distribution.Utils.String (decodeStringUtf8, encodeStringUtf8)
import Distribution.Utils.Structured (Structured (..), nominalStructure)

#if defined(MIN_VERSION_bytestring)
# if MIN_VERSION_bytestring(0,10,4)
# define HAVE_SHORTBYTESTRING 1
# endif
#endif

-- Hack for GHC bootstrapping
--
-- Currently (as of GHC 8.1), GHC bootstraps Cabal by building
-- binary and Cabal in one giant ghc --make command.  This
-- means no MIN_VERSION_binary macro is available.
--
-- We could try to cleverly figure something out in this case,
-- but there is a better plan: just use the unoptimized version
-- of the Binary instance.  We're not going to use it for anything
-- real in any case.
--
-- WARNING: Don't use MIN_VERSION_binary to smooth over a BC-break!
--
#ifndef MIN_VERSION_binary
#define MIN_VERSION_binary(x, y, z) 0
#endif

import qualified Data.ByteString as BS
import qualified Data.List as List

#if HAVE_SHORTBYTESTRING
import qualified Data.ByteString.Short as BS.Short
#else
import Distribution.Utils.Generic (fromUTF8BS)
#endif

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
-- 'BS.Short.ShortByteString' when compiled against @bytestring >=
-- 0.10.4@, and otherwise the fallback is to use plain old non-compat
-- '[Char]'.
--
-- Note: This type is for internal uses (such as e.g. 'PackageName')
-- and shall not be exposed in Cabal's API
--
-- @since 2.0.0.2
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

unsafeFromUTF8BS = ST . BS.Short.toShort

null = BS.Short.null . unST
#else
newtype ShortText = ST { unST :: String }
                  deriving (Eq,Ord,Generic,Data,Typeable)

instance Binary ShortText where
    put = put . encodeStringUtf8 . unST
    get = fmap (ST . decodeStringUtf8) get

toShortText = ST

fromShortText = unST

unsafeFromUTF8BS = ST . fromUTF8BS

null = List.null . unST
#endif

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
