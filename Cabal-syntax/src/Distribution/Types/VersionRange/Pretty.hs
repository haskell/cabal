{-# LANGUAGE BangPatterns #-}
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

import Debug.Pretty.Simple

import Data.Map (Map)
import qualified Data.Map as M
import Distribution.Types.Annotation
import Distribution.Types.Namespace

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
prettyVersionRange :: TriviaTree -> VersionRange -> Disp.Doc
prettyVersionRange t0 vr = prettyVersionRange' t0 0 vr

prettyVersionRange' :: TriviaTree -> Int -> VersionRange -> Disp.Doc 
prettyVersionRange' t0 d vr =
  pTrace ("prettyVersionRange' t0\n" <> show t0)
  $ case vr of
    v0@(ThisVersion v)       -> let t = unmark (SomeNamespace v0) t0 in triviaToDoc (justAnnotation t0) $ Disp.text "==" <<>> prettier t v
    v0@(LaterVersion v)      -> let t = unmark (SomeNamespace v0) t0 in triviaToDoc (justAnnotation t0) $ Disp.text ">" <<>> prettier t v
    v0@(OrLaterVersion v)    -> let t = unmark (SomeNamespace v0) t0 in triviaToDoc (justAnnotation t0) $ Disp.text ">=" <<>> prettier t v
    v0@(EarlierVersion v)    -> let t = unmark (SomeNamespace v0) t0 in triviaToDoc (justAnnotation t0) $ Disp.text "<" <<>> prettier t v
    v0@(OrEarlierVersion v)  -> let t = unmark (SomeNamespace v0) t0 in triviaToDoc (justAnnotation t0) $ Disp.text "<=" <<>> prettier t v
    v0@(MajorBoundVersion v) -> let t = unmark (SomeNamespace v0) t0 in triviaToDoc (justAnnotation t0) $ Disp.text "^>=" <<>> prettier t v

    v@(UnionVersionRanges r1 r2) ->
        let t = unmark (SomeNamespace v) t0
        in triviaToDoc (justAnnotation t0)
            $ parens (d > 0)
            $ prettyVersionRange' t (d+1) r1 <+> Disp.text "||" <+> prettyVersionRange' t (d+0) r2
    v@(IntersectVersionRanges r1 r2) ->
        let t = unmark (SomeNamespace v) t0
        in    triviaToDoc (justAnnotation t0)
              $ parens (d > 1)
              $ prettyVersionRange' t (d+2) r1 <+> Disp.text "&&" <+> prettyVersionRange' t (d+1) r2
  where
    parens True = Disp.parens
    parens False = id

-- | Don't use && and || operators. If possible.
prettyVersionRange16 :: TriviaTree -> VersionRange -> Disp.Doc
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
