module Distribution.Types.AnnotationTrivium where

data Trivium
  = FieldNth Int
  | Nth Int
  | PreTrivia String
  | PostTrivia String
