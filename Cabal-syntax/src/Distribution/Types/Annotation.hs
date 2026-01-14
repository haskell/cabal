{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Distribution.Types.Annotation where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import qualified Text.PrettyPrint as Disp

import Control.DeepSeq

-- TODO: import all the types that we need to use as key to index the trivia

import Distribution.Types.Dependency.Internal
import Distribution.Types.LibraryName.Internal
import Distribution.Types.PackageName.Internal
import Distribution.Types.Version.Internal
import Distribution.Types.VersionRange

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
  TriviaTree locala belowa <> TriviaTree localb belowb =
    TriviaTree (locala <> localb)
      (M.unionWith (<>) belowa belowb) -- The (<>) of map is clobbering, we need to use (<>) to join the values

instance Monoid TriviaTree where
  mempty = emptyTriviaTree

fromNamedTrivia :: Namespace -> Trivia -> TriviaTree
fromNamedTrivia ns ts = TriviaTree mempty (M.singleton ns (TriviaTree ts mempty))

emptyTriviaTree :: TriviaTree
emptyTriviaTree = TriviaTree mempty mempty

-- | Wrap the trivia within a namespace
mark :: Namespace -> TriviaTree -> TriviaTree
mark ns ts = TriviaTree mempty (M.singleton ns ts)

-- | If the trivia map is for this scope
unmark :: Namespace -> TriviaTree -> TriviaTree
unmark ns tt = fromMaybe mempty (M.lookup ns (namedAnnotations tt))

triviaToDoc :: Trivia -> Disp.Doc -> Disp.Doc
triviaToDoc [] x = x
triviaToDoc (t:ts) x = triviaToDoc ts (triviumToDoc t x)

triviumToDoc :: Trivium -> Disp.Doc -> Disp.Doc
triviumToDoc t x = case t of
  PreTrivia s -> Disp.text s <> x
  PostTrivia s -> x <> Disp.text s
  IsInjected -> mempty -- the doc in question shouldn't be rendered
