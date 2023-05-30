{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.ComponentId
  ( ComponentId
  , unComponentId
  , mkComponentId
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P
import Text.PrettyPrint (text)

-- | A 'ComponentId' uniquely identifies the transitive source
-- code closure of a component (i.e. libraries, executables).
--
-- For non-Backpack components, this corresponds one to one with
-- the 'UnitId', which serves as the basis for install paths,
-- linker symbols, etc.
--
-- Use 'mkComponentId' and 'unComponentId' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype ComponentId = ComponentId ShortText
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Construct a 'ComponentId' from a 'String'
--
-- 'mkComponentId' is the inverse to 'unComponentId'
--
-- Note: No validations are performed to ensure that the resulting
-- 'ComponentId' is valid
--
-- @since 2.0.0.2
mkComponentId :: String -> ComponentId
mkComponentId = ComponentId . toShortText

-- | Convert 'ComponentId' to 'String'
--
-- @since 2.0.0.2
unComponentId :: ComponentId -> String
unComponentId (ComponentId s) = fromShortText s

-- | 'mkComponentId'
--
-- @since 2.0.0.2
instance IsString ComponentId where
  fromString = mkComponentId

instance Binary ComponentId
instance Structured ComponentId

instance Pretty ComponentId where
  pretty = text . unComponentId

instance Parsec ComponentId where
  parsec = mkComponentId `fmap` P.munch1 abi_char
    where
      abi_char c = isAlphaNum c || c `elem` "-_."

instance NFData ComponentId where
  rnf = rnf . unComponentId
