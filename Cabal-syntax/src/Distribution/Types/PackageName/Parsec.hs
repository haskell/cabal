module Distribution.Types.PackageName.Parsec where

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import Distribution.Parsec.Class
import Distribution.Types.PackageName
import Distribution.CabalParsing

instance Parsec PackageName where
  parsec = do
    x <- mkPackageName <$> parsecUnqualComponentName
    annotate
      ( Section "library" "" $ Section "build-depends" "" $ NSPackageName x
      )
      (PreTrivia "fake trivia")

    pure x
