{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.HashValue
  ( HashValue
  , hashValue
  , truncateHash
  , showHashValue
  , showHashValueBase64
  , readFileHashValue
  , hashFromTUF
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Hackage.Security.Client as Sec

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
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
  deriving (Eq, Generic, Show, Typeable)

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

showHashValue :: HashValue -> String
showHashValue (HashValue digest) = BS.unpack (Base16.encode digest)

showHashValueBase64 :: HashValue -> String
showHashValueBase64 (HashValue digest) = BS.unpack (Base64.encode digest)

-- | Hash the content of a file. Uses SHA256.
readFileHashValue :: FilePath -> IO HashValue
readFileHashValue tarball =
  withBinaryFile tarball ReadMode $ \hnd ->
    evaluate . hashValue =<< LBS.hGetContents hnd

-- | Convert a hash from TUF metadata into a 'PackageSourceHash'.
--
-- Note that TUF hashes don't necessarily have to be SHA256, since it can
-- support new algorithms in future.
{- FOURMOLU_DISABLE -}
hashFromTUF :: Sec.Hash -> HashValue
hashFromTUF (Sec.Hash hashstr) =
  -- TODO: [code cleanup] either we should get TUF to use raw bytestrings or
  -- perhaps we should also just use a base16 string as the internal rep.
  case Base16.decode (BS.pack hashstr) of
#if MIN_VERSION_base16_bytestring(1,0,0)
      Right hash -> HashValue hash
      Left _ -> error "hashFromTUF: cannot decode base16"
#else
      (hash, trailing) | not (BS.null hash) && BS.null trailing
        -> HashValue hash
      _ -> error "hashFromTUF: cannot decode base16 hash"
#endif
{- FOURMOLU_ENABLE -}

-- | Truncate a 32 byte SHA256 hash to
--
-- For example 20 bytes render as 40 hex chars, which we use for unit-ids.
-- Or even 4 bytes for 'hashedInstalledPackageIdShort'
truncateHash :: Int -> HashValue -> HashValue
truncateHash n (HashValue h) = HashValue (BS.take n h)
