{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Types.ExecutableScope (
    ExecutableScope(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Pretty
import Distribution.Parsec
import Distribution.FieldGrammar.Described

import qualified Distribution.Compat.CharParsing as P
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

instance Described ExecutableScope where
    describe _ = REUnion ["public","private"]

instance Binary ExecutableScope
instance Structured ExecutableScope
instance NFData ExecutableScope where rnf = genericRnf

-- | 'Any' like semigroup, where 'ExecutablePrivate' is 'Any True'
instance Semigroup ExecutableScope where
    ExecutablePublic    <> x = x
    x@ExecutablePrivate <> _ = x

-- | 'mempty' = 'ExecutablePublic'
instance Monoid ExecutableScope where
    mempty = ExecutablePublic
    mappend = (<>)
