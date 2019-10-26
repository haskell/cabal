{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.Script (
    Script(..)
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Executable

data Script = Script {
    executable :: Executable,
    hcPath :: Maybe FilePath
    }
    deriving (Generic, Show, Read, Eq, Typeable)