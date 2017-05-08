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

data ExecutableScope = ExecutableScopeUnknown
                     | ExecutablePublic
                     | ExecutablePrivate
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Text ExecutableScope where
  disp ExecutablePublic  = text "public"
  disp ExecutablePrivate = text "private"
  disp ExecutableScopeUnknown = text "unknown"

  parse = Parse.choice
    [ Parse.string "public"  >> return ExecutablePublic
    , Parse.string "private" >> return ExecutablePrivate
    ]

instance Binary ExecutableScope

instance Monoid ExecutableScope where
    mempty = ExecutableScopeUnknown
    mappend = (<>)

instance Semigroup ExecutableScope where
    ExecutableScopeUnknown <> x = x
    x <> ExecutableScopeUnknown = x
    x <> y | x == y             = x
           | otherwise          = error "Ambiguous executable scope"
