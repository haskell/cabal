module Distribution.Types.PackageName.Parsec where

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import Distribution.Parsec.Class
import Distribution.Types.PackageName
import Distribution.CabalParsing

instance Parsec PackageName where
  parsec = do
    annotate
      (Section "library" "" $ Field "build-depends"
        -- TODO: put the parsed component name inside the namespace
      )
      (PreTrivia "fake trivia")
    mkPackageName <$> parsecUnqualComponentName
