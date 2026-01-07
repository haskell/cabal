module Distribution.Types.PackageName.Parsec where

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import Distribution.Parsec.Class
import Distribution.Types.PackageName.Internal
import Distribution.CabalParsing

instance Parsec PackageName where
  parsec = mkPackageName <$> parsecUnqualComponentName
