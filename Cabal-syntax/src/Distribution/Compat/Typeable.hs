{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Compat.Typeable
  ( Typeable
  , TypeRep
  , typeRep
  ) where

#if MIN_VERSION_base(4,7,0)
import Data.Typeable (Typeable, TypeRep, typeRep)
#else
import Data.Typeable (Typeable, TypeRep, typeOf)
#endif

#if !MIN_VERSION_base(4,7,0)
typeRep :: forall a proxy. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
#endif
