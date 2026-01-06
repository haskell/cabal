module Distribution.Types.UnqualComponentName.Parsec where

import Distribution.Parsec
import Distribution.Types.UnqualComponentName

instance Parsec UnqualComponentName where
  parsec = mkUnqualComponentName <$> parsecUnqualComponentName
