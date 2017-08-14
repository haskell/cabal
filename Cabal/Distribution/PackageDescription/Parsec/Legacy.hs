{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- @since 2.2.0.0
module Distribution.PackageDescription.Parsec.Legacy (patchLegacy) where

import           Prelude ()
import           Distribution.Compat.Prelude
import           GHC.Fingerprint (Fingerprint (..), fingerprintData)
import           Foreign.Ptr (castPtr)
import           System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map as Map

-- | Patch legacy @.cabal@ file contents to allow parsec parser to accept
-- all of Hackage.
--
-- Bool part of the result tells whether the output is modified.
--
-- @since 2.2.0.0
patchLegacy :: BS.ByteString -> (Bool, BS.ByteString)
patchLegacy bs = case Map.lookup (BS.take 256 bs, md5 bs) patches of
    Nothing -> (False, bs)
    Just (post, f)
        | post /= md5 output -> (False, bs)
        | otherwise          -> (True, output)
      where
        output = f bs

md5 :: BS.ByteString -> Fingerprint
md5 bs = unsafeDupablePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fingerprintData (castPtr ptr) len

-- | 'patches' contains first 256 bytes, pre- and post-fingerprints and a patch function.
--
--
patches :: Map.Map (BS.ByteString, Fingerprint) (Fingerprint, BS.ByteString -> BS.ByteString)
patches = Map.fromList
    -- http://hackage.haskell.org/package/unicode-transforms-0.3.3
    -- other-modules: .
    [ mk "-- This file has been generated from package.yaml by hpack version 0.17.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:                unicode-transforms\nversion:             0.3.3\nsynopsis:            Unicode normalization\ndescription:         Fast Unic"
         (Fingerprint 15958160436627155571 10318709190730872881)
         (Fingerprint 11008465475756725834 13815629925116264363)
         (bsRemove "  other-modules:\n      .\n") -- TODO: remove traling \n to test structural-diff
    ]
  where
    mk a b c d = ((a, b), (c, d))

-- | Helper to create entries in patches
_makePatchKey :: FilePath -> (BS.ByteString -> BS.ByteString) -> NoCallStackIO ()
_makePatchKey fp transform = do
    contents <- BS.readFile fp
    let output = transform contents
    let Fingerprint hi lo = md5 contents
    let Fingerprint hi' lo' = md5 output
    putStrLn 
        $ showString "mk "
        . shows (BS.take 256 contents)
        . showString "\n   (Fingerprint "
        . shows hi
        . showString " "
        . shows lo
        . showString ")\n   (Fingerprint "
        . shows hi'
        . showString " "
        . shows lo'
        . showString ")"
        $ ""

-------------------------------------------------------------------------------
-- Patch helpers
-------------------------------------------------------------------------------

bsRemove
    :: BS.ByteString  -- ^ needle
    -> BS.ByteString -> BS.ByteString
bsRemove needle haystack = case BS.breakSubstring needle haystack of
    (h, t) -> BS.append h (BS.drop (BS.length needle) t)
