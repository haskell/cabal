module Distribution.Types.Annotation where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE

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
  deriving (Show)

data Namespace
  = NSVersion Version
  | NSVersionRange VersionRange
  | NSPackageName PackageName
  | NSDependency Dependency
  | NSField BS.ByteString
  | NSLibrarySection LibraryName
  deriving (Eq, Ord, Show)

-- data TriviaTree
--   = Node Namespace Trivia (NE.NonEmpty TriviaTree)
--   | Leaf Namespace Trivia
--
-- instance Semigroup TriviaTree where
--   Node nsa ta <> Node nsb tb
--     | nsa == nsb = Node nsa (ta <> tb)
--     | otherwise = Node 
--
-- trivia :: Namespace -> TriviaTree -> Trivia
-- trivia ns0 tt = case tt of
--   Node ns t ts | ns == ns0 -> t
--   Leaf ns t | ns == ns0 -> t
--   _ -> mempty
--
-- mark :: Namespace -> TriviaTree -> TriviaTree
-- mark ns tt = Node ns mempty (pure tt)
--
-- unmark :: Namespace -> TriviaTree -> (NE.NonEmpty TriviaTree)
-- unmark ns0 tt = case tt of
--   Node ns t ts | ns == ns0 -> ts
--   _ -> mempty

-- TODO(leana8959): can I encode namespace type in the signature or somehow make this more type safe?
data TriviaTree = TriviaTree
  { annotationsLocal :: Trivia
  , annotationsBelow :: Map Namespace TriviaTree
  }
  deriving (Show)

instance Semigroup TriviaTree where
  TriviaTree locala belowa <> TriviaTree localb belowb = TriviaTree (locala <> localb) (belowa <> belowb)

instance Monoid TriviaTree where
  mempty = emptyTriviaTree

emptyTriviaTree :: TriviaTree
emptyTriviaTree = TriviaTree mempty mempty

-- | Get the trivia of this scope
trivia :: TriviaTree -> Trivia
trivia = annotationsLocal

annotateTriviaTree :: Namespace -> Trivia -> TriviaTree -> TriviaTree
annotateTriviaTree ns t (TriviaTree local below) =
  TriviaTree local (M.insertWith (<>) ns (TriviaTree t mempty) below)

-- | Wrap the trivia within a namespace
mark :: Namespace -> TriviaTree -> TriviaTree
mark ns tt = TriviaTree mempty (M.singleton ns tt)

-- | If the trivia map is for this scope
unmark :: Namespace -> TriviaTree -> TriviaTree
unmark ns tt = fromMaybe mempty (M.lookup ns (annotationsBelow tt))
