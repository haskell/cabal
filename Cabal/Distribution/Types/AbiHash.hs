{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.AbiHash
  ( AbiHash, unAbiHash, mkAbiHash
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import qualified Distribution.Compat.CharParsing as P
import Distribution.Pretty
import Distribution.Parsec
import Distribution.FieldGrammar.Described

import Text.PrettyPrint (text)

-- | ABI Hashes
--
-- Use 'mkAbiHash' and 'unAbiHash' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype AbiHash = AbiHash ShortText
    deriving (Eq, Show, Read, Generic, Typeable)

-- | Construct a 'AbiHash' from a 'String'
--
-- 'mkAbiHash' is the inverse to 'unAbiHash'
--
-- Note: No validations are performed to ensure that the resulting
-- 'AbiHash' is valid
--
-- @since 2.0.0.2
unAbiHash :: AbiHash -> String
unAbiHash (AbiHash h) = fromShortText h

-- | Convert 'AbiHash' to 'String'
--
-- @since 2.0.0.2
mkAbiHash :: String -> AbiHash
mkAbiHash = AbiHash . toShortText

-- | 'mkAbiHash'
--
-- @since 2.0.0.2
instance IsString AbiHash where
    fromString = mkAbiHash

instance Binary AbiHash
instance Structured AbiHash
instance NFData AbiHash where rnf = genericRnf

instance Pretty AbiHash where
    pretty = text . unAbiHash

instance Parsec AbiHash where
    parsec = fmap mkAbiHash (P.munch isAlphaNum)

instance Described AbiHash where
    describe _ = reMunchCS csAlphaNum 
