{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.CommonStanzaImports (
  CommonStanzaImports(..),

  emptyCommonStanzaImports,
) where

import Prelude ()
import Distribution.Compat.Prelude


import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.UnqualComponentName (UnqualComponentName, mkUnqualComponentName)
import qualified Text.PrettyPrint                as Disp
import Distribution.FieldGrammar.Described


-- | Represents the list of common stanzas specified in the `import` directive
--   of sections in a cabal file.
data CommonStanzaImports = CommonStanzaImports {
  -- | The names of common stanzas to be imported.
  getCommonStanzaImports :: [UnqualComponentName]
  }
  deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary CommonStanzaImports
instance Structured CommonStanzaImports
instance NFData CommonStanzaImports where rnf = genericRnf

instance Monoid CommonStanzaImports where
  mempty = CommonStanzaImports {
    getCommonStanzaImports = []
    }
  mappend = (<>)

instance Semigroup CommonStanzaImports where
  a <> b = CommonStanzaImports {
    getCommonStanzaImports = getCommonStanzaImports a <> getCommonStanzaImports b
    }

emptyCommonStanzaImports :: CommonStanzaImports
emptyCommonStanzaImports = mempty

instance Pretty CommonStanzaImports where
  pretty (CommonStanzaImports []) = Disp.empty
  pretty (CommonStanzaImports imports) =
    Disp.fsep (Disp.punctuate Disp.comma (map pretty imports))

instance Parsec CommonStanzaImports where
  parsec = CommonStanzaImports <$> parsecLeadingCommaList parsec

instance Described CommonStanzaImports where
  describe _ = RETodo
