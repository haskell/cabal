{-# LANGUAGE ExistentialQuantification #-}

module Distribution.Types.Namespace where

import Data.Typeable
import qualified Data.ByteString as BS

-- The Show constraint is simply for debugging
class (Typeable a, Show a, Eq a) => Namespace a

instance Namespace Char
instance Namespace BS.ByteString

instance Namespace a => Namespace [a]
instance (Namespace a, Namespace b) => Namespace (a, b)
instance (Namespace a, Namespace b, Namespace c) => Namespace (a, b, c)
instance (Namespace a, Namespace b, Namespace c, Namespace d) => Namespace (a, b, c, d)
instance (Namespace a, Namespace b, Namespace c, Namespace d, Namespace e) => Namespace (a, b, c, d, e)

data SomeNamespace = forall a. Namespace a => SomeNamespace a

fromNamespace :: Namespace a => SomeNamespace -> Maybe a
fromNamespace (SomeNamespace ns) = cast ns

isNamespace :: Namespace a => a -> SomeNamespace -> Bool
isNamespace a someB = maybe False (==a) (fromNamespace someB)
