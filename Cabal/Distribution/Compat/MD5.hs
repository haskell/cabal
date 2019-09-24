module Distribution.Compat.MD5 (
    MD5,
    showMD5,
    md5,
    -- * Binary
    binaryPutMD5,
    binaryGetMD5,
    ) where

import Data.Binary      (Get, Put)
import Data.Binary.Get  (getWord64le)
import Data.Binary.Put  (putWord64le)
import Foreign.Ptr      (castPtr)
import GHC.Fingerprint  (Fingerprint (..), fingerprintData)
import Numeric          (showHex)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS

type MD5 = Fingerprint

-- | Show 'MD5' in human readable form
--
-- >>> showMD5 (Fingerprint 123 456)
-- "000000000000007b00000000000001c8"
--
-- >>> showMD5 $ md5 $ BS.pack [0..127]
-- "37eff01866ba3f538421b30b7cbefcac"
--
showMD5 :: MD5 -> String
showMD5 (Fingerprint a b) = pad a' ++ pad b' where
    a' = showHex a ""
    b' = showHex b ""
    pad s = replicate (16 - length s) '0' ++ s

md5 :: BS.ByteString -> MD5
md5 bs = unsafeDupablePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fingerprintData (castPtr ptr) len

binaryPutMD5 :: MD5 -> Put
binaryPutMD5 (Fingerprint a b) = do
    putWord64le a
    putWord64le b

binaryGetMD5 :: Get MD5
binaryGetMD5 = do
    a <- getWord64le
    b <- getWord64le
    return (Fingerprint a b)
