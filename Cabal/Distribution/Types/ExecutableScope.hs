{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ExecutableScope (
    ExecutableScope(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

data ExecutableScope = ExecutablePublic
                     | ExecutablePrivate
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Pretty ExecutableScope where
    pretty ExecutablePublic       = Disp.text "public"
    pretty ExecutablePrivate      = Disp.text "private"

instance Parsec ExecutableScope where
    parsec = P.try pub <|> pri where
        pub = ExecutablePublic  <$ P.string "public"
        pri = ExecutablePrivate <$ P.string "private"

instance Text ExecutableScope where
    parse = Parse.choice
        [ Parse.string "public"  >> return ExecutablePublic
        , Parse.string "private" >> return ExecutablePrivate
        ]

instance Binary ExecutableScope

instance NFData ExecutableScope where rnf = genericRnf

-- | 'Any' like semigroup, where 'ExecutablePrivate' is 'Any True'
instance Semigroup ExecutableScope where
    ExecutablePublic    <> x = x
    x@ExecutablePrivate <> _ = x

-- | 'mempty' = 'ExecutablePublic'
instance Monoid ExecutableScope where
    mempty = ExecutablePublic
    mappend = (<>)
