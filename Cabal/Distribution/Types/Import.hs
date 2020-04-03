{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.CommonStanzaImports (
  CommonStanzaImports(..),

  emptyCommonStanzaImports,
) where

import Prelude ()
import Distribution.Compat.Prelude

-- | Represents the list of common stanzas specified in the `import` directive
--   of sections in a cabal file.
data CommonStanzaImports = CommonStanzaImports {
  -- | The names of common stanzas to be imported.
  commonStanzaImports :: [String]
  }
  deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary BuildInfo
instance Structured BuildInfo
instance NFData BuildInfo where rnf = genericRnf

instance Monoid BuildInfo where
  mempty = BuildInfo {
    commonStanzaImports = [],
    }
  mappend = (<>)

instance Semigroup CommonStanzaImports where
  a <> b = CommonStanzaImports {
    commonStanzaImports = commonStanzaImports a <> commonStanzaImports b
    }
