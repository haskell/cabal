module Distribution.Client.HashValue
  ( HashValue
  , hashValue
  , truncateHash
  , showHashValue
  , readFileHashValue
  , hashFromTUF
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Hackage.Security.Client as Sec

import Distribution.Simple.HashValue

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS

-- | Convert a hash from TUF metadata into a 'PackageSourceHash'.
--
-- Note that TUF hashes don't necessarily have to be SHA256, since it can
-- support new algorithms in future.
hashFromTUF :: Sec.Hash -> HashValue
hashFromTUF (Sec.Hash hashstr) =
  -- TODO: [code cleanup] either we should get TUF to use raw bytestrings or
  -- perhaps we should also just use a base16 string as the internal rep.
  case Base16.decode (BS.pack hashstr) of
    Right hash -> rawHashValue hash
    Left _ -> error "hashFromTUF: cannot decode base16"
