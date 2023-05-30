{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.PkgconfigVersionRange
  ( PkgconfigVersionRange (..)
  , anyPkgconfigVersion
  , isAnyPkgconfigVersion
  , withinPkgconfigVersionRange

    -- * Internal
  , versionToPkgconfigVersion
  , versionRangeToPkgconfigVersionRange
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.PkgconfigVersion
import Distribution.Types.Version
import Distribution.Types.VersionInterval
import Distribution.Types.VersionRange

import qualified Data.ByteString.Char8 as BS8
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

-- | @since 3.0
data PkgconfigVersionRange
  = PcAnyVersion
  | PcThisVersion PkgconfigVersion -- = version
  | PcLaterVersion PkgconfigVersion -- > version  (NB. not >=)
  | PcEarlierVersion PkgconfigVersion -- < version
  | PcOrLaterVersion PkgconfigVersion -- >= version
  | PcOrEarlierVersion PkgconfigVersion -- =< version
  | PcUnionVersionRanges PkgconfigVersionRange PkgconfigVersionRange
  | PcIntersectVersionRanges PkgconfigVersionRange PkgconfigVersionRange
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PkgconfigVersionRange
instance Structured PkgconfigVersionRange
instance NFData PkgconfigVersionRange where rnf = genericRnf

instance Pretty PkgconfigVersionRange where
  pretty = pp 0
    where
      pp :: Int -> PkgconfigVersionRange -> PP.Doc
      pp _ PcAnyVersion = PP.text "-any"
      pp _ (PcThisVersion v) = PP.text "==" <<>> pretty v
      pp _ (PcLaterVersion v) = PP.text ">" <<>> pretty v
      pp _ (PcEarlierVersion v) = PP.text "<" <<>> pretty v
      pp _ (PcOrLaterVersion v) = PP.text ">=" <<>> pretty v
      pp _ (PcOrEarlierVersion v) = PP.text "<=" <<>> pretty v
      pp d (PcUnionVersionRanges v u) =
        parens (d >= 1) $
          pp 1 v PP.<+> PP.text "||" PP.<+> pp 0 u
      pp d (PcIntersectVersionRanges v u) =
        parens (d >= 2) $
          pp 2 v PP.<+> PP.text "&&" PP.<+> pp 1 u

      parens True = PP.parens
      parens False = id

instance Parsec PkgconfigVersionRange where
  -- note: the wildcard is used in some places, e.g
  -- http://hackage.haskell.org/package/bindings-libzip-0.10.1/bindings-libzip.cabal
  --
  -- however, in the presence of alphanumerics etc. lax version parser,
  -- wildcard is ill-specified

  parsec = do
    csv <- askCabalSpecVersion
    if csv >= CabalSpecV3_0
      then pkgconfigParser
      else versionRangeToPkgconfigVersionRange <$> versionRangeParser P.integral csv

-- "modern" parser of @pkg-config@ package versions.
pkgconfigParser :: CabalParsing m => m PkgconfigVersionRange
pkgconfigParser = P.spaces >> expr
  where
    -- every parser here eats trailing space
    expr = do
      ts <- term `P.sepByNonEmpty` (P.string "||" >> P.spaces)
      return $ foldr1 PcUnionVersionRanges ts

    term = do
      fs <- factor `P.sepByNonEmpty` (P.string "&&" >> P.spaces)
      return $ foldr1 PcIntersectVersionRanges fs

    factor = parens expr <|> prim

    prim = do
      op <- P.munch1 isOpChar P.<?> "operator"
      case op of
        "-" -> anyPkgconfigVersion <$ (P.string "any" *> P.spaces)
        "==" -> afterOp PcThisVersion
        ">" -> afterOp PcLaterVersion
        "<" -> afterOp PcEarlierVersion
        ">=" -> afterOp PcOrLaterVersion
        "<=" -> afterOp PcOrEarlierVersion
        _ -> P.unexpected $ "Unknown version operator " ++ show op

    -- https://gitlab.haskell.org/ghc/ghc/issues/17752
    isOpChar '<' = True
    isOpChar '=' = True
    isOpChar '>' = True
    isOpChar '^' = True
    isOpChar '-' = True
    isOpChar _ = False

    afterOp f = do
      P.spaces
      v <- parsec
      P.spaces
      return (f v)

    parens =
      P.between
        ((P.char '(' P.<?> "opening paren") >> P.spaces)
        (P.char ')' >> P.spaces)

anyPkgconfigVersion :: PkgconfigVersionRange
anyPkgconfigVersion = PcAnyVersion

-- | TODO: this is not precise, but used only to prettify output.
isAnyPkgconfigVersion :: PkgconfigVersionRange -> Bool
isAnyPkgconfigVersion = (== PcAnyVersion)

withinPkgconfigVersionRange :: PkgconfigVersion -> PkgconfigVersionRange -> Bool
withinPkgconfigVersionRange v = go
  where
    go PcAnyVersion = True
    go (PcThisVersion u) = v == u
    go (PcLaterVersion u) = v > u
    go (PcEarlierVersion u) = v < u
    go (PcOrLaterVersion u) = v >= u
    go (PcOrEarlierVersion u) = v <= u
    go (PcUnionVersionRanges a b) = go a || go b
    go (PcIntersectVersionRanges a b) = go a && go b

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

versionToPkgconfigVersion :: Version -> PkgconfigVersion
versionToPkgconfigVersion = PkgconfigVersion . BS8.pack . prettyShow

versionRangeToPkgconfigVersionRange :: VersionRange -> PkgconfigVersionRange
versionRangeToPkgconfigVersionRange vr
  | isAnyVersion vr =
      PcAnyVersion
  | otherwise =
      case asVersionIntervals vr of
        [] -> PcEarlierVersion (PkgconfigVersion (BS8.pack "0"))
        (i : is) -> foldl (\r j -> PcUnionVersionRanges r (conv j)) (conv i) is
  where
    conv (VersionInterval (LowerBound v b) NoUpperBound) = convL v b
    conv (VersionInterval (LowerBound v b) (UpperBound u c)) = PcIntersectVersionRanges (convL v b) (convU u c)

    convL v ExclusiveBound = PcLaterVersion (versionToPkgconfigVersion v)
    convL v InclusiveBound = PcOrLaterVersion (versionToPkgconfigVersion v)

    convU v ExclusiveBound = PcEarlierVersion (versionToPkgconfigVersion v)
    convU v InclusiveBound = PcOrEarlierVersion (versionToPkgconfigVersion v)
