{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Support for binary serialization with a fingerprint.
module Distribution.Utils.BinaryWithFingerprint (
    encodeWithFingerprint,
    decodeWithFingerprint,
    decodeWithFingerprintOrFailIO,
) where

#if MIN_VERSION_base(4,8,0)

import Distribution.Compat.Binary
import Data.ByteString.Lazy (ByteString)
import Control.Exception

import Data.Binary.Get
import Data.Binary.Put

import GHC.Generics
import GHC.Fingerprint
import Data.Typeable
import Control.Monad

-- | Private wrapper type so we can give 'Binary' instance for
-- 'Fingerprint'
newtype FP = FP Fingerprint

instance Binary FP where
    put (FP (Fingerprint a b)) = put a >> put b
    get = do
        a <- get
        b <- get
        return (FP (Fingerprint a b))

fingerprintRep :: forall a. Typeable (Rep a) => Proxy a -> Fingerprint
fingerprintRep _ = typeRepFingerprint (typeRep (Proxy :: Proxy (Rep a)))

-- | Encode a value, recording a fingerprint in the header.
--
-- The fingerprint is GHC's Typeable fingerprint associated with
-- the Generic Rep of a type: this fingerprint is better than
-- the fingerprint of the type itself, as it changes when the
-- representation changes (and thus the binary serialization format
-- changes.)
--
encodeWithFingerprint :: forall a. (Binary a, Typeable (Rep a)) => a -> ByteString
encodeWithFingerprint x = runPut $ do
    put (FP (fingerprintRep (Proxy :: Proxy a)))
    put x

-- | Decode a value, verifying the fingerprint in the header.
--
decodeWithFingerprint :: forall a. (Binary a, Typeable (Rep a)) => ByteString -> a
decodeWithFingerprint = runGet $ do
    FP fp <- get
    let expect_fp = fingerprintRep (Proxy :: Proxy a)
    when (expect_fp /= fp) $
        fail $ "Expected fingerprint " ++ show expect_fp ++
               " but got " ++ show fp
    get

-- | Decode a value, forcing the decoded value to discover decoding errors
-- and report them.
--
decodeWithFingerprintOrFailIO :: (Binary a, Typeable (Rep a)) => ByteString -> IO (Either String a)
decodeWithFingerprintOrFailIO bs =
  catch (evaluate (decodeWithFingerprint bs) >>= return . Right)
  $ \(ErrorCall str) -> return $ Left str

#else

import Distribution.Compat.Binary
import Data.ByteString.Lazy (ByteString)

-- Dummy implementations that don't actually save fingerprints

encodeWithFingerprint :: Binary a => a -> ByteString
encodeWithFingerprint = encode

decodeWithFingerprint :: Binary a => ByteString -> a
decodeWithFingerprint = decode

decodeWithFingerprintOrFailIO :: Binary a => ByteString -> IO (Either String a)
decodeWithFingerprintOrFailIO = decodeOrFailIO

#endif
