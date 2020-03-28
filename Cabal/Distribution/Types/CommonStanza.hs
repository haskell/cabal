{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.CommonStanza (
    CommonStanza(..),
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.BuildInfo
import Distribution.Types.UnqualComponentName

import qualified Distribution.Types.BuildInfo.Lens as L

data CommonStanza = CommonStanza
    { commonStanzaName      :: UnqualComponentName
    , commonStanzaBuildInfo :: BuildInfo
    }
    deriving (Generic, Show, Eq, Read, Typeable, Data)

instance L.HasBuildInfo CommonStanza where
    buildInfo f l = (\x -> l { commonStanzaBuildInfo = x }) <$> f (commonStanzaBuildInfo l)

instance Binary CommonStanza
instance Structured CommonStanza
instance NFData CommonStanza where rnf = genericRnf
