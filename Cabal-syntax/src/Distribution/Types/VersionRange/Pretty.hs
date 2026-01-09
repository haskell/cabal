{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | The only purpose of this module is to prevent the export of
-- 'VersionRange' constructors from
-- "Distribution.Types.VersionRange". To avoid creating orphan
-- instances, a lot of related code had to be moved here too.
module Distribution.Types.VersionRange.Pretty
  ( VersionRange (..)
  , anyVersion
  , noVersion
  , thisVersion
  , notThisVersion
  , laterVersion
  , earlierVersion
  , orLaterVersion
  , orEarlierVersion
  , unionVersionRanges
  , intersectVersionRanges
  , withinVersion
  , majorBoundVersion
  , VersionRangeF (.., LEUpperBound, GTLowerBound, TZUpperBound)
  , projectVersionRange
  , embedVersionRange
  , cataVersionRange
  , anaVersionRange
  , hyloVersionRange
  -- , versionRangeParser
  , majorUpperBound
  , wildcardUpperBound
  ) where

import Distribution.Compat.Prelude
import Distribution.Types.Version.Internal
import Distribution.Types.Version.Pretty
import Distribution.Types.VersionRange.Internal
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Pretty
import Distribution.Utils.Generic (unsnoc)

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Text.PrettyPrint as Disp

import Data.Map (Map)
import qualified Data.Map as M
import Distribution.Types.Annotation

-- |
--
-- >>> fmap pretty (simpleParsec' CabalSpecV1_6 "== 3.2.*" :: Maybe VersionRange)
-- Just >=3.2 && <3.3
--
-- >>> fmap (prettyVersioned CabalSpecV1_6) (simpleParsec' CabalSpecV1_6 "== 3.2.*" :: Maybe VersionRange)
-- Just ==3.2.*
--
-- >>> fmap pretty (simpleParsec' CabalSpecV1_6 "-any" :: Maybe VersionRange)
-- Just >=0
--
-- >>> fmap (prettyVersioned CabalSpecV1_6) (simpleParsec' CabalSpecV1_6 "-any" :: Maybe VersionRange)
-- Just >=0

instance Pretty VersionRange where
  prettier = prettierVersioned cabalSpecLatest

  prettierVersioned csv
    | csv > CabalSpecV1_6 = prettyVersionRange
    | otherwise = prettyVersionRange16

-- TODO(leana8959): unpack trivium we have saved
prettyVersionRange :: Map Namespace [Trivium] -> VersionRange -> Disp.Doc
prettyVersionRange t0 vr = cataVersionRange alg vr 0
  where
    unwrap :: Namespace -> Namespace
    unwrap (NSVersionRange vRange (Just s)) | vRange == vr = s
    unwrap s = s

    t = M.mapKeys unwrap t0

    alg :: VersionRangeF (Int -> Disp.Doc) -> Int -> Disp.Doc
    alg (ThisVersionF v) _ = Disp.text "==" <<>> prettier t v
    alg (LaterVersionF v) _ = Disp.text ">" <<>> prettier t v
    alg (OrLaterVersionF v) _ = Disp.text ">=" <<>> prettier t v
    alg (EarlierVersionF v) _ = Disp.text "<" <<>> prettier t v
    alg (OrEarlierVersionF v) _ = Disp.text "<=" <<>> prettier t v
    alg (MajorBoundVersionF v) _ = Disp.text "^>=" <<>> prettier t v
    alg (UnionVersionRangesF r1 r2) d =
      parens (d > 0) $
        r1 1 <+> Disp.text "||" <+> r2 0
    alg (IntersectVersionRangesF r1 r2) d =
      parens (d > 1) $
        r1 2 <+> Disp.text "&&" <+> r2 1

    parens True = Disp.parens
    parens False = id

-- | Don't use && and || operators. If possible.
prettyVersionRange16 :: Map Namespace [Trivium] -> VersionRange -> Disp.Doc
prettyVersionRange16 t (IntersectVersionRanges (OrLaterVersion v) (EarlierVersion u))
  | u == wildcardUpperBound v =
      Disp.text "==" <<>> dispWild v
  where
    dispWild ver =
      Disp.hcat
        ( Disp.punctuate
            (Disp.char '.')
            (map Disp.int $ versionNumbers ver)
        )
        <<>> Disp.text ".*"
prettyVersionRange16 t vr = prettyVersionRange t vr
