{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.SPDX.LicenseExpression
  ( LicenseExpression (..)
  , SimpleLicenseExpression (..)
  , simpleLicenseExpression
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.SPDX.LicenseExceptionId
import Distribution.SPDX.LicenseId
import Distribution.SPDX.LicenseListVersion
import Distribution.SPDX.LicenseReference
import Distribution.Utils.Generic (isAsciiAlphaNum)

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

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
  = ELicense !SimpleLicenseExpression !(Maybe LicenseExceptionId)
  | EAnd !LicenseExpression !LicenseExpression
  | EOr !LicenseExpression !LicenseExpression
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | Simple License Expressions.
data SimpleLicenseExpression
  = -- | An SPDX License List Short Form Identifier. For example: @GPL-2.0-only@
    ELicenseId LicenseId
  | -- | An SPDX License List Short Form Identifier with a unary"+" operator suffix to represent the current version of the license or any later version.  For example: @GPL-2.0+@
    ELicenseIdPlus LicenseId
  | -- | A SPDX user defined license reference: For example: @LicenseRef-23@, @LicenseRef-MIT-Style-1@, or @DocumentRef-spdx-tool-1.2:LicenseRef-MIT-Style-2@
    ELicenseRef LicenseRef
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

simpleLicenseExpression :: LicenseId -> LicenseExpression
simpleLicenseExpression i = ELicense (ELicenseId i) Nothing

instance Binary LicenseExpression
instance Binary SimpleLicenseExpression
instance Structured SimpleLicenseExpression
instance Structured LicenseExpression

instance Pretty LicenseExpression where
  pretty = go 0
    where
      go :: Int -> LicenseExpression -> Disp.Doc
      go _ (ELicense lic exc) =
        let doc = pretty lic
         in maybe id (\e d -> d <+> Disp.text "WITH" <+> pretty e) exc doc
      go d (EAnd e1 e2) = parens (d < 0) $ go 0 e1 <+> Disp.text "AND" <+> go 0 e2
      go d (EOr e1 e2) = parens (d < 1) $ go 1 e1 <+> Disp.text "OR" <+> go 1 e2

      parens False doc = doc
      parens True doc = Disp.parens doc

instance Pretty SimpleLicenseExpression where
  pretty (ELicenseId i) = pretty i
  pretty (ELicenseIdPlus i) = pretty i <<>> Disp.char '+'
  pretty (ELicenseRef r) = pretty r

instance Parsec SimpleLicenseExpression where
  parsec = idstring >>= simple
    where
      simple n
        | Just l <- "LicenseRef-" `isPrefixOfMaybe` n =
            maybe (fail $ "Incorrect LicenseRef format: " ++ n) (return . ELicenseRef) $ mkLicenseRef Nothing l
        | Just d <- "DocumentRef-" `isPrefixOfMaybe` n = do
            _ <- P.string ":LicenseRef-"
            l <- idstring
            maybe (fail $ "Incorrect LicenseRef format:" ++ n) (return . ELicenseRef) $ mkLicenseRef (Just d) l
        | otherwise = do
            v <- askCabalSpecVersion
            l <-
              maybe (fail $ "Unknown SPDX license identifier: '" ++ n ++ "' " ++ licenseIdMigrationMessage n) return $
                mkLicenseId (cabalSpecVersionToSPDXListVersion v) n
            orLater <- isJust <$> P.optional (P.char '+')
            if orLater
              then return (ELicenseIdPlus l)
              else return (ELicenseId l)

idstring :: P.CharParsing m => m String
idstring = P.munch1 $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'

-- returns suffix part
isPrefixOfMaybe :: Eq a => [a] -> [a] -> Maybe [a]
isPrefixOfMaybe pfx s
  | pfx `isPrefixOf` s = Just (drop (length pfx) s)
  | otherwise = Nothing

instance Parsec LicenseExpression where
  parsec = expr
    where
      expr = compoundOr

      simple = do
        s <- parsec
        exc <- exception
        return $ ELicense s exc

      exception = P.optional $ P.try (spaces1 *> P.string "WITH" *> spaces1) *> parsec

      compoundOr = do
        x <- compoundAnd
        l <- P.optional $ P.try (spaces1 *> P.string "OR" *> spaces1) *> compoundOr
        return $ maybe id (flip EOr) l x

      compoundAnd = do
        x <- compound
        l <- P.optional $ P.try (spaces1 *> P.string "AND" *> spaces1) *> compoundAnd
        return $ maybe id (flip EAnd) l x

      compound = braces <|> simple

      -- NOTE: we require that there's a space around AND & OR operators,
      -- i.e. @(MIT)AND(MIT)@ will cause parse-error.
      braces = do
        _ <- P.char '('
        _ <- P.spaces
        x <- expr
        _ <- P.char ')'
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
  rnf (ELicense s e) = rnf s `seq` rnf e
  rnf (EAnd x y) = rnf x `seq` rnf y
  rnf (EOr x y) = rnf x `seq` rnf y

instance NFData SimpleLicenseExpression where
  rnf (ELicenseId i) = rnf i
  rnf (ELicenseIdPlus i) = rnf i
  rnf (ELicenseRef r) = rnf r
