module Distribution.PPUserState where

import Data.ByteString (ByteString)
import Data.Char (digitToInt, intToDigit)
import Data.List (transpose)
import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Distribution.Parsec.Error (PError (..), PErrorWithSource (..), showPError, showPErrorWithSource)

import Data.Monoid (Last (..))
import Distribution.Parsec.FieldLineStream (FieldLineStream, fieldLineStreamFromBS, fieldLineStreamFromString)
import Distribution.Parsec.Position (Position (..), incPos, retPos, showPos, zeroPos)
import Distribution.Parsec.Warning
import Numeric (showIntAtBase)
import Prelude ()

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Distribution.Compat.MonadFail as Fail
import qualified Text.Parsec as Parsec

import qualified Data.Map as Map



data PPUserState = PPUserState
  { ppWarnings :: [PWarning]
  , ppTrivia :: Map Namespace [Trivium] -- TODO(leana8959): make this a map
  }

emptyPPUserState :: PPUserState
emptyPPUserState = PPUserState [] Map.empty
