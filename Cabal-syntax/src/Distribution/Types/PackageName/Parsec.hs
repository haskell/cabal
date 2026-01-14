module Distribution.Types.PackageName.Parsec where

import Distribution.Types.Annotation

import Distribution.CabalParsing
import Distribution.Parsec.Class
import Distribution.Types.PackageName.Internal

instance Parsec PackageName where
  parsec = mkPackageName <$> parsecUnqualComponentName
