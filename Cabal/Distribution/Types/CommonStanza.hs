{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.CommonStanza (
    CommonStanza(..),
    emptyCommonStanza,
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.BuildInfo
import Distribution.Types.CommonStanzaImports
import Distribution.Types.UnqualComponentName

import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.CommonStanzaImports.Lens as L

data CommonStanza = CommonStanza
    { commonStanzaName      :: UnqualComponentName
    , commonStanzaRecursiveImports:: CommonStanzaImports
    , commonStanzaBuildInfo :: BuildInfo
    }
    deriving (Generic, Show, Eq, Read, Typeable, Data)

instance L.HasCommonStanzaImports CommonStanza where
    commonStanzaImports f l = (\x -> l { commonStanzaRecursiveImports = x }) <$> f (commonStanzaRecursiveImports l)

instance L.HasBuildInfo CommonStanza where
    buildInfo f l = (\x -> l { commonStanzaBuildInfo = x }) <$> f (commonStanzaBuildInfo l)

instance Binary CommonStanza
instance Structured CommonStanza
instance NFData CommonStanza where rnf = genericRnf

instance Monoid CommonStanza where
  mempty = gmempty
  mappend = (<>)

instance Semigroup CommonStanza where
  a <> b = CommonStanza{
    commonStanzaName = combine' commonStanzaName,
    commonStanzaRecursiveImports = combine commonStanzaRecursiveImports,
    commonStanzaBuildInfo = combine commonStanzaBuildInfo
  }
    where combine field = field a `mappend` field b
          combine' field = case ( unUnqualComponentName $ field a
                                , unUnqualComponentName $ field b) of
                      ("", _) -> field b
                      (_, "") -> field a
                      (x, y) -> error $ "Ambiguous values for executable field: '"
                                  ++ x ++ "' and '" ++ y ++ "'"

emptyCommonStanza :: CommonStanza
emptyCommonStanza = mempty
