{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.FieldGrammar.Described (
    Described (..),
    describeDoc,
    -- * Regular expressions
    Regex (..),
    reEps,
    reChar,
    reChars,
    reMunchCS,
    reMunch1CS,
    -- * Variables
    reVar0,
    reVar1,
    -- * Special expressions
    reDot,
    reComma,
    reSpacedComma,
    reHsString,
    reUnqualComponent,
    -- * Lists
    reSpacedList,
    reCommaList,
    reOptCommaList,
    -- * Character Sets
    csChar,
    csAlphaNum,
    csNotSpace,
    csNotSpaceOrComma,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec (Parsec)
import Distribution.Pretty (Pretty)

import Distribution.Utils.Regex

import qualified Distribution.Utils.CharSet as CS
import qualified Text.PrettyPrint           as PP

-- | Class describing the pretty/parsec format of a.
class (Pretty a, Parsec a) => Described a where
    -- | A pretty document of "regex" describing the field format
    describe :: proxy a -> Regex void

-- | Pretty-print description.
--
-- >>> describeDoc ([] :: [Bool])
-- \left\{ \mathop{\mathord{``}\mathtt{True}\mathord{"}}\mid\mathop{\mathord{``}\mathtt{False}\mathord{"}} \right\}
--
describeDoc :: Described a => proxy a -> PP.Doc
describeDoc p = regexDoc (describe p)

instance Described Bool where
    describe _ = REUnion ["True", "False"]

instance Described a => Described (Identity a) where
    describe _ = describe ([] :: [a])

-------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

reSpacedList :: Regex a -> Regex a
reSpacedList = REMunch RESpaces1

reCommaList :: Regex a -> Regex a
reCommaList = RECommaList

reOptCommaList :: Regex a -> Regex a
reOptCommaList = REOptCommaList

-------------------------------------------------------------------------------
-- Specific grammars
-------------------------------------------------------------------------------

reHsString :: Regex a
reHsString = RENamed "hs-string" impl  where
    impl = reChar '"' <> REMunch reEps (REUnion [strChar, escChar]) <> reChar '"'
    strChar = RECharSet $ CS.difference CS.universe (CS.fromList "\"\\")

    escChar = REUnion
        [ "\\&"
        , "\\\\"
        , REUnion ["\\n", RENamed "escapes" "\\n"] -- TODO
        , "\\" <> RECharSet "0123456789"
        , "\\o" <> RECharSet "01234567"
        , "\\x" <> RECharSet "0123456789abcdefABCDEF"
        , REUnion ["\\^@", RENamed "control" "\\^@"] -- TODO
        , REUnion ["\\NUL", RENamed "ascii" "\\NUL"] -- TODO
        ]

reUnqualComponent :: Regex a
reUnqualComponent = RENamed "unqual-name" $
    REMunch1 (reChar '-') component
  where
    component
        = REMunch reEps (RECharSet csAlphaNum)
        -- currently the parser accepts "csAlphaNum `difference` "0123456789"
        -- which is larger set than CS.alpha
        --
        -- Hackage rejects non ANSI names, so it's not so relevant.
        <> RECharSet CS.alpha
        <> REMunch reEps (RECharSet csAlphaNum)

reDot :: Regex a
reDot = reChar '.'

reComma :: Regex a
reComma = reChar ','

reSpacedComma :: Regex a
reSpacedComma = RESpaces <> reComma <> RESpaces

-------------------------------------------------------------------------------
-- Character sets
-------------------------------------------------------------------------------

csChar :: Char -> CS.CharSet
csChar = CS.singleton

csAlphaNum :: CS.CharSet
csAlphaNum = CS.alphanum

csNotSpace :: CS.CharSet
csNotSpace = CS.difference CS.universe $ CS.singleton ' '

csNotSpaceOrComma :: CS.CharSet
csNotSpaceOrComma = CS.difference csNotSpace $ CS.singleton ','
