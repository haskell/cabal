module Distribution.Types.UnqualComponentName.Parsec where

import Distribution.Parsec
import Distribution.Types.UnqualComponentName.Internal

instance Parsec UnqualComponentName where
  parsec = mkUnqualComponentName <$> parsecUnqualComponentName
