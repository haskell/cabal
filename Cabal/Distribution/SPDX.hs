{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | This module contains a SPDX data from specification version 2.1
--
-- Specification is available on <https://spdx.org/specifications>
module Distribution.SPDX (
    -- * License expression
    LicenseExpression (..),
    simpleLicenseExpression,
    OnlyOrAnyLater (..),
    -- * License identifier
    LicenseId (..),
    licenseId,
    licenseName,
    licenseIsOsiApproved,
    mkLicenseId,
    -- * License exception
    LicenseExceptionId (..),
    licenseExceptionId,
    licenseExceptionName,
    mkLicenseExceptionId,
    -- * License reference
    LicenseRef,
    licenseRef,
    licenseDocumentRef,
    mkLicenseRef,
    mkLicenseRef',
    unsafeMkLicenseRef,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.SPDX.LicenseExceptionId
import Distribution.SPDX.LicenseId
import Distribution.SPDX.LicenseReference
import Distribution.Utils.Generic           (isAsciiAlphaNum)
import Text.PrettyPrint                     ((<+>))

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint                as Disp

-- | SPDX License Expression.
--
-- @
-- idstring              = 1*(ALPHA \/ DIGIT \/ "-" \/ "." )
-- license id            = \<short form license identifier inAppendix I.1>
-- license exception id  = \<short form license exception identifier inAppendix I.2>
-- license ref           = [\"DocumentRef-"1*(idstring)":"]\"LicenseRef-"1*(idstring)
--
-- simple expression     = license id \/ license id"+" \/ license ref
--
-- compound expression   = 1*1(simple expression \/
--                         simple expression \"WITH" license exception id \/
--                         compound expression \"AND" compound expression \/
--                         compound expression \"OR" compound expression ) \/
--                         "(" compound expression ")" )
--
-- license expression    = 1*1(simple expression / compound expression)
-- @
data LicenseExpression
    = ELicense !(Either LicenseRef LicenseId) !OnlyOrAnyLater !(Maybe LicenseExceptionId)
    | EAnd !LicenseExpression !LicenseExpression
    | EOr !LicenseExpression !LicenseExpression
    deriving (Show, Read, Eq, Typeable, Data, Generic)

simpleLicenseExpression :: LicenseId -> LicenseExpression
simpleLicenseExpression i = ELicense (Right i) Only Nothing

instance Binary LicenseExpression

instance Pretty LicenseExpression where
    pretty = go 0
      where
        go :: Int -> LicenseExpression -> Disp.Doc
        go _ (ELicense lic orLater exc) =
            let doc = prettyId lic <<>> prettyOrLater orLater
            in maybe id (\e d -> d <+> Disp.text "WITH" <+> pretty e) exc doc
        go d (EAnd e1 e2) = parens (d < 0) $ go 0 e1 <+> Disp.text "AND" <+> go 0 e2
        go d (EOr  e1 e2) = parens (d < 1) $ go 1 e1 <+> Disp.text "OR" <+> go 1 e2

        prettyId (Right i) = pretty i
        prettyId (Left r)  = pretty r

        prettyOrLater Only       = mempty
        prettyOrLater OrAnyLater = Disp.char '+'

        parens False doc = doc
        parens True  doc = Disp.parens doc

instance Parsec LicenseExpression where
    parsec = expr
      where
        expr = compoundOr

        idstring = P.munch1 $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'

        -- this parses "simple expression / simple expression "WITH" license exception id"
        simple = do
            n <- idstring
            i <- simple' n
            orLater <- P.optional $ P.char '+'
            _ <- P.spaces
            exc <- P.optional $ P.try (P.string "WITH" *> spaces1) *> parsec
            return $ ELicense i (maybe Only (const OrAnyLater) orLater) exc

        simple' n
            | Just l <- "LicenseRef-" `isPrefixOfMaybe` n =
                maybe (fail $ "Incorrect LicenseRef format: " ++ n) (return . Left) $ mkLicenseRef Nothing l
            | Just d <- "DocumentRef-" `isPrefixOfMaybe` n = do
                _ <- P.string ":LicenseRef"
                l <- idstring
                maybe (fail $ "Incorrect LicenseRef format:" ++ n) (return . Left) $ mkLicenseRef (Just d) l
            | otherwise =
                maybe (fail $ "Unknown SPDX license identifier: " ++ n) (return . Right) $ mkLicenseId n

        -- returns suffix part
        isPrefixOfMaybe :: Eq a => [a] -> [a] -> Maybe [a]
        isPrefixOfMaybe pfx s
            | pfx `isPrefixOf` s = Just (drop (length pfx) s)
            | otherwise          = Nothing

        compoundOr = do
            x <- compoundAnd
            l <- P.optional $ P.try (P.string "OR" *> spaces1) *> compoundOr
            return $ maybe id (flip EOr) l x

        compoundAnd = do
            x <- compound
            l <- P.optional $ P.try (P.string "AND" *> spaces1) *> compoundAnd
            return $ maybe id (flip EAnd) l x

        compound = braces <|> simple

        braces = do
            _ <- P.char '('
            _ <- P.spaces
            x <- expr
            _ <- P.char ')'
            _ <- P.spaces
            return x

        spaces1 = P.space *> P.spaces

-- notes:
--
-- There MUST NOT be whitespace between a license­id and any following "+".  This supports easy parsing and
-- backwards compatibility.  There MUST be whitespace on either side of the operator "WITH".  There MUST be
-- whitespace and/or parentheses on either side of the operators "AND" and "OR".
--
-- We handle that by having greedy 'idstring' parser, so MITAND would parse as invalid license identifier.

instance NFData LicenseExpression where
    rnf (ELicense b i e) = rnf b `seq` rnf i `seq` rnf e
    rnf (EAnd x y)       = rnf x `seq` rnf y
    rnf (EOr x y)        = rnf x `seq` rnf y

-------------------------------------------------------------------------------
-- OnlyOrAnyLater
-------------------------------------------------------------------------------

-- | License version range.
data OnlyOrAnyLater = Only | OrAnyLater
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)

instance NFData OnlyOrAnyLater where
    rnf Only       = ()
    rnf OrAnyLater = ()

instance Binary OnlyOrAnyLater
