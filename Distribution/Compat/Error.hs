{-# OPTIONS -cpp #-}
module Distribution.Compat.Error (Error(..)) where

#ifndef __NHC__
import Control.Monad.Error (Error(..))
#endif

#ifdef __NHC__
class Error e where
        strMsg :: String -> e

-- This is a horrible hack, but H98 doesn't allow
-- instance Error [Char]
instance Error Char where
        strMsg s = head s
instance Error e => Error [e] where
        strMsg s = map (strMsg . (:[])) s

instance Error e => Monad (Either e) where
        return = Right
        fail   = Left . strMsg
        Left e  >>= f = Left e
        Right x >>= f = f x
#endif
