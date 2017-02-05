{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ExecutableScope (
    ExecutableScope(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Text
import qualified Distribution.Compat.ReadP as Parse

import Text.PrettyPrint (text)

data ExecutableScope = ExecutablePublic
                     | ExecutablePrivate
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Text ExecutableScope where
  disp ExecutablePublic  = text "public"
  disp ExecutablePrivate = text "private"

  parse = Parse.choice
    [ Parse.string "public"  >> return ExecutablePublic
    , Parse.string "private" >> return ExecutablePrivate
    ]

instance Binary ExecutableScope

instance Monoid ExecutableScope where
    mempty = ExecutablePublic
    mappend = (<>)

instance Semigroup ExecutableScope where
    x <> y | x /= y    = error "Ambiguous executable scope"
           | otherwise = x
