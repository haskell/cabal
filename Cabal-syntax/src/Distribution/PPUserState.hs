module Distribution.PPUserState where

import Distribution.Compat.Prelude

import Distribution.Parsec.Warning
import Prelude ()

import Distribution.Types.Annotation

import qualified Data.Map as Map


data PPUserState = PPUserState
  { ppWarnings :: [PWarning]
  , ppTrivia :: TriviaTree
  }

emptyPPUserState :: PPUserState
emptyPPUserState = PPUserState [] mempty
