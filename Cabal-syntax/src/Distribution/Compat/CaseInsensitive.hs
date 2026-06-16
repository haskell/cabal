module Distribution.Compat.CaseInsensitive where

import Data.String (IsString (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)

class FoldCase s where
  foldCase :: s -> s

instance FoldCase ByteString where
  foldCase = BS8.map toLower

instance FoldCase Char where
  foldCase = toLower

instance FoldCase a => FoldCase [a] where
  foldCase = map foldCase

data CaseInsensitive a = MkCaseInsensitive { unCaseInsensitive :: !a }

foldedCase :: FoldCase a => CaseInsensitive a -> a
foldedCase (MkCaseInsensitive s) = foldCase s

instance IsString a => IsString (CaseInsensitive a) where
  fromString = MkCaseInsensitive . fromString

instance (FoldCase a, Eq a) => Eq (CaseInsensitive a) where
  MkCaseInsensitive a == MkCaseInsensitive b = foldCase a == foldCase b

instance (FoldCase a, Ord a) => Ord (CaseInsensitive a) where
  MkCaseInsensitive a `compare` MkCaseInsensitive b = foldCase a `compare` foldCase b
