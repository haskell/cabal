{-# LANGUAGE ExistentialQuantification #-}

module Distribution.Types.Namespace where

import Data.Typeable
import qualified Data.ByteString as BS

class (Typeable a, Show a, Eq a) => Namespace a

instance Namespace BS.ByteString

data SomeNamespace = forall a. Namespace a => SomeNamespace a

fromNamespace :: Namespace a => SomeNamespace -> Maybe a
fromNamespace (SomeNamespace ns) = cast ns

isNamespace :: Namespace a => a -> SomeNamespace -> Bool
isNamespace a someB = maybe False (==a) (fromNamespace someB)
