{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.Annotation where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE

import Control.DeepSeq

-- TODO: import all the types that we need to use as key to index the trivia
import Distribution.Types.VersionRange
import Distribution.Types.Version.Internal
import Distribution.Types.PackageName.Internal
import Distribution.Types.Dependency.Internal
import Distribution.Types.LibraryName.Internal

type Trivia = [Trivium]
data Trivium
  = FieldNth Int
  | Nth Int
  | PreTrivia String
  | PostTrivia String
  | IsInjected
  deriving (Generic, Show)

instance NFData Trivium

data Namespace
  = NSVersion Version
  | NSVersionRange VersionRange
  | NSPackageName PackageName
  | NSDependency Dependency
  | NSField BS.ByteString
  | NSLibrarySection LibraryName
  deriving (Eq, Ord, Generic, Show)

instance NFData Namespace

-- TODO(leana8959): can I encode namespace type in the signature or somehow make this more type safe?
data TriviaTree = TriviaTree
  { justAnnotation :: Trivia
  , namedAnnotations :: Map Namespace TriviaTree
  }
  deriving (Generic, Show)

instance NFData TriviaTree

instance Semigroup TriviaTree where
  TriviaTree locala belowa <> TriviaTree localb belowb = TriviaTree (locala <> localb) (belowa <> belowb)

instance Monoid TriviaTree where
  mempty = emptyTriviaTree

fromNamedTrivia :: Namespace -> Trivia -> TriviaTree
fromNamedTrivia ns ts = annotateTriviaTree ns ts mempty

emptyTriviaTree :: TriviaTree
emptyTriviaTree = TriviaTree mempty mempty

-- | Get the trivia of this scope
trivia :: TriviaTree -> Trivia
trivia = justAnnotation

annotateTriviaTree :: Namespace -> Trivia -> TriviaTree -> TriviaTree
annotateTriviaTree ns t (TriviaTree local below) =
  TriviaTree local (M.insertWith (<>) ns (TriviaTree t mempty) below)

annotateTriviaTreeLocal :: Trivia -> TriviaTree -> TriviaTree
annotateTriviaTreeLocal t (TriviaTree local below) =
  TriviaTree (t <> local) below

-- | Wrap the trivia within a namespace
mark :: Namespace -> [TriviaTree] -> TriviaTree
mark ns tts = TriviaTree mempty (M.fromList $ zip (repeat ns) tts)

-- | If the trivia map is for this scope
unmark :: Namespace -> TriviaTree -> TriviaTree
unmark ns tt = fromMaybe mempty (M.lookup ns (namedAnnotations tt))
