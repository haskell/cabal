module Compat.H98 where

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
