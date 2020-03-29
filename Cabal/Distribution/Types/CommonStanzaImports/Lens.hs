module Distribution.Types.CommonStanzaImports.Lens (
    CommonStanzaImports,
    HasCommonStanzaImports (..),
    ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.Types.CommonStanzaImports (CommonStanzaImports)

-- | Class lenses for 'CommonStanzaImports'.
class HasCommonStanzaImports a where
    commonStanzaImports :: Lens' a CommonStanzaImports

instance HasCommonStanzaImports CommonStanzaImports where
    commonStanzaImports = id
    {-# INLINE commonStanzaImports #-}
