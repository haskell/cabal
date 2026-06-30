{-# LANGUAGE DeriveGeneric #-}

module Distribution.Simple.HashValue
  ( HashValue
  , hashValue
  , hashEncode
  , rawHashValue
  , truncateHash
  , showHashValue
  , readFileHashValue
  ) where

import Distribution.Compat.Prelude

import Control.Monad ( (<=<) )
import qualified Data.Binary as Binary
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import System.IO (IOMode (..), withBinaryFile)

-----------------------------------------------
-- The specific choice of hash implementation
--

-- Is a crypto hash necessary here? One thing to consider is who controls the
-- inputs and what's the result of a hash collision. Obviously we should not
-- install packages we don't trust because they can run all sorts of code, but
-- if I've checked there's no TH, no custom Setup etc, is there still a
-- problem? If someone provided us a tarball that hashed to the same value as
-- some other package and we installed it, we could end up re-using that
-- installed package in place of another one we wanted. So yes, in general
-- there is some value in preventing intentional hash collisions in installed
-- package ids.

newtype HashValue = HashValue BS.ByteString
  deriving (Eq, Generic, Show)

-- Cannot do any sensible validation here. Although we use SHA256
-- for stuff we hash ourselves, we can also get hashes from TUF
-- and that can in principle use different hash functions in future.
--
-- Therefore, we simply derive this structurally.
instance Binary HashValue
instance Structured HashValue

-- | Hash some data. Currently uses SHA256.
hashValue :: LBS.ByteString -> HashValue
hashValue = HashValue . SHA256.hashlazy

-- | Hash a value's 'Binary' representation.
hashEncode :: Binary a => a -> HashValue
hashEncode = hashValue . Binary.encode

-- | Convert a raw hash value, given as a bytestring, into a 'HashValue'. No
-- well-formedness guarantees are provided; the caller is responsible for
-- ensuring that the provided hash is valid.
rawHashValue :: BS.ByteString -> HashValue
rawHashValue = HashValue

showHashValue :: HashValue -> String
showHashValue (HashValue digest) = BS.unpack (Base16.encode digest)

-- | Hash the content of a file. Uses SHA256.
readFileHashValue :: FilePath -> IO HashValue
readFileHashValue tarball =
  withBinaryFile tarball ReadMode $
    evaluate . hashValue <=< LBS.hGetContents
-- | Truncate a 32 byte SHA256 hash to
--
-- For example 20 bytes render as 40 hex chars, which we use for unit-ids.
-- Or even 4 bytes for 'hashedInstalledPackageIdShort'
truncateHash :: Int -> HashValue -> HashValue
truncateHash n (HashValue h) = HashValue (BS.take n h)
