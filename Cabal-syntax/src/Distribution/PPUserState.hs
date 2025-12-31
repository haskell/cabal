module Distribution.PPUserState where

import Distribution.Compat.Prelude

import Distribution.Parsec.Warning
import Prelude ()

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import qualified Data.Map as Map



data PPUserState = PPUserState
  { ppWarnings :: [PWarning]
  , ppTrivia :: Map Namespace [Trivium] -- TODO(leana8959): make this a map
  }

emptyPPUserState :: PPUserState
emptyPPUserState = PPUserState [] Map.empty
