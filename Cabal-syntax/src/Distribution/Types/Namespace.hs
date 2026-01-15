{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Distribution.Types.Namespace where

import Control.DeepSeq
import Data.Typeable
import qualified Data.ByteString as BS

class
  ( Typeable a
  , Eq a
  , Ord a
  , Show a -- The Show constraint is simply for debugging
  ) => Namespace a
instance Namespace Char
instance Namespace BS.ByteString
instance Namespace a => Namespace [a]
instance (Namespace a, Namespace b) => Namespace (a, b)
instance (Namespace a, Namespace b, Namespace c) => Namespace (a, b, c)
instance (Namespace a, Namespace b, Namespace c, Namespace d) => Namespace (a, b, c, d)
instance (Namespace a, Namespace b, Namespace c, Namespace d, Namespace e) => Namespace (a, b, c, d, e)

data SomeNamespace = forall a. Namespace a => SomeNamespace a
deriving instance Show SomeNamespace

instance Eq SomeNamespace where
  (SomeNamespace x) == (SomeNamespace y) = case cast x of
    Just x' -> x' == y
    Nothing -> False

-- TODO(leana8959): is this bad
instance Ord SomeNamespace where
  (SomeNamespace x) <= (SomeNamespace y) = case cast x of
    Just x' -> x' <= y
    Nothing -> False

fromNamespace :: Namespace a => SomeNamespace -> Maybe a
fromNamespace (SomeNamespace ns) = cast ns

isNamespace :: Namespace a => a -> SomeNamespace -> Bool
isNamespace a someB = maybe False (==a) (fromNamespace someB)
