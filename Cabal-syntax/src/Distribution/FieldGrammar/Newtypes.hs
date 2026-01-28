{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides @newtype@ wrappers to be used with "Distribution.FieldGrammar".
module Distribution.FieldGrammar.Newtypes
  ( -- * List
    alaList
  , alaList'

    -- ** Modifiers
  , CommaVCat (..)
  , CommaFSep (..)
  , VCat (..)
  , FSep (..)
  , NoCommaFSep (..)
  , Sep (..)
  , TriviaSep (..)

    -- ** Type
  , List

    -- ** Set
  , alaSet
  , alaSet'
  , Set'

    -- ** NonEmpty
  , alaNonEmpty
  , alaNonEmpty'
  , NonEmpty'

    -- * Version & License
  , SpecVersion (..)
  , TestedWith (..)
  , SpecLicense (..)

    -- * Identifiers
  , Token (..)
  , Token' (..)
  , MQuoted (..)
  , FilePathNT (..)
  , SymbolicPathNT (..)
  , RelativePathNT (..)
  ) where

import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Prelude ()

import Debug.Pretty.Simple
import Distribution.CabalSpecVersion
import Distribution.Compiler (CompilerFlavor)
import Distribution.License (License)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Fields.Pretty
import Distribution.ExactPrettyField
import Distribution.Utils.Path
import Distribution.Version
  ( LowerBound (..)
  , Version
  , VersionInterval (..)
  , VersionRange
  , VersionRangeF (..)
  , anyVersion
  , asVersionIntervals
  , cataVersionRange
  , mkVersion
  , version0
  , versionNumbers
  )
import Text.PrettyPrint (Doc, comma, fsep, punctuate, text, vcat)

import Distribution.Types.Annotation

import qualified Data.Map as M
import Data.List (sortOn, groupBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.SPDX as SPDX

-- | Vertical list with commas. Displayed with 'vcat'
data CommaVCat = CommaVCat
  deriving (Generic)

-- | Paragraph fill list with commas. Displayed with 'fsep'
data CommaFSep = CommaFSep

-- | Vertical list with optional commas. Displayed with 'vcat'.
data VCat = VCat

-- | Paragraph fill list with optional commas. Displayed with 'fsep'.
data FSep = FSep

-- | Paragraph fill list without commas. Displayed with 'fsep'.
data NoCommaFSep = NoCommaFSep

-- Trivia counterparts associate trivia associated with each data
class Sep sep where
  prettySep :: Proxy sep -> [Doc] -> Doc

  parseSep :: CabalParsing m => Proxy sep -> m a -> m [a]
  parseSepNE :: CabalParsing m => Proxy sep -> m a -> m (NonEmpty a)

class TriviaSep sep where
  prettierSep
    :: (ExactPretty a)
    => Proxy sep -> [(TriviaTree, a)] -> Doc

  triviaParseSep
    :: (CabalParsing m, Markable a, Namespace a)
    => Proxy sep -> m (TriviaTree, a) -> m [(TriviaTree, a)]

  triviaParseSepNE
    :: (CabalParsing m, Markable a, Namespace a)
    => Proxy sep -> m (TriviaTree, a) -> m (NonEmpty (TriviaTree, a))

-- NOTE(leana8959): Dependency List is such a type
instance TriviaSep CommaVCat where
  -- FIXME(leana8959):
  prettierSep _ docs =
    let docs' =
          map
            ( \(t, x) -> triviaToDoc (justAnnotation t) $ exactPretty t x
            )
          $ sortOn (fromMaybe 0 . atNth . justAnnotation . fst)
          $ docs
        -- NOTE(leana8959): we don't "punctuate" anymore
        -- The separators are in the trivia
    in  mconcat docs'

  triviaParseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then triviaParsecLeadingCommaList p else triviaParsecCommaList p

instance TriviaSep CommaFSep where
  prettierSep _ = fsep . punctuate comma . map (uncurry exactPretty)

  triviaParseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then triviaParsecLeadingCommaList p else triviaParsecCommaList p

  triviaParseSepNE _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then triviaParsecLeadingCommaListNonEmpty p else triviaParsecCommaNonEmpty p

instance TriviaSep VCat where
  prettierSep _ = vcat . map (uncurry exactPretty)

  triviaParseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV3_0 then triviaParsecLeadingOptCommaList p else triviaParsecOptCommaList p

  -- TODO(leana8959):
  triviaParseSepNE _ p = NE.some1 (p <* P.spaces)

instance TriviaSep FSep where
  prettierSep _ = fsep . map (uncurry exactPretty)
  triviaParseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV3_0 then triviaParsecLeadingOptCommaList p else triviaParsecOptCommaList p

  -- TODO(leana8959):
  triviaParseSepNE _ p = NE.some1 (p <* P.spaces)

instance TriviaSep NoCommaFSep where
  prettierSep _ = fsep . map (uncurry exactPretty)
  triviaParseSep _ p = many (p <* P.spaces)
  triviaParseSepNE _ p = NE.some1 (p <* P.spaces)

instance Sep CommaVCat where
  prettySep _ = vcat . punctuate comma
  parseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then parsecLeadingCommaList p else parsecCommaList p
  parseSepNE _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then parsecLeadingCommaNonEmpty p else parsecCommaNonEmpty p
instance Sep CommaFSep where
  prettySep _ = fsep . punctuate comma
  parseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then parsecLeadingCommaList p else parsecCommaList p
  parseSepNE _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then parsecLeadingCommaNonEmpty p else parsecCommaNonEmpty p
instance Sep VCat where
  prettySep _ = vcat
  parseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV3_0 then parsecLeadingOptCommaList p else parsecOptCommaList p
  parseSepNE _ p = NE.some1 (p <* P.spaces)
instance Sep FSep where
  prettySep _ = fsep
  parseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV3_0 then parsecLeadingOptCommaList p else parsecOptCommaList p
  parseSepNE _ p = NE.some1 (p <* P.spaces)
instance Sep NoCommaFSep where
  prettySep _ = fsep
  parseSep _ p = many (p <* P.spaces)
  parseSepNE _ p = NE.some1 (p <* P.spaces)

-- | List separated with optional commas. Displayed with @sep@, arguments of
-- type @a@ are parsed and pretty-printed as @b@.
newtype List sep b a = List {_getList :: [a]}
  deriving (Show, Ord, Eq, Generic)

-- | 'alaList' and 'alaList'' are simply 'List', with additional phantom
-- arguments to constrain the resulting type
--
-- >>> :t alaList VCat
-- alaList VCat :: [a] -> List VCat (Identity a) a
--
-- >>> :t alaList' FSep Token
-- alaList' FSep Token :: [String] -> List FSep Token String
alaList :: sep -> [a] -> List sep (Identity a) a
alaList _ = List

-- | More general version of 'alaList'.
alaList' :: sep -> (a -> b) -> [a] -> List sep b a
alaList' _ _ = List

instance Newtype [a] (List sep wrapper a)

instance
  ( Newtype a b
  , Typeable sep
  , TriviaSep sep
  , ExactParsec b
  , Namespace a
  , Typeable b
  , Ord b
  , Show b
  , Markable b
  ) => Parsec (List sep b a) where
  parsec = snd <$> exactParsec

-- Multiplicity within fields
instance
  ( Newtype a b
  , Typeable sep
  , TriviaSep sep
  , ExactParsec b
  , Markable b
  , Namespace b
  ) => ExactParsec (List sep b a) where
  exactParsec = do
    (ts, bs) <- unzip <$> triviaParseSep (Proxy :: Proxy sep) exactParsec
    pure (mconcat ts, pack $ map (unpack :: b -> a) bs)

instance (Newtype a b, Sep sep, Pretty b) => Pretty (List sep b a) where
  pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (pack :: a -> b)) . unpack

-- The inner type is parsed as b, so we wrap it and mark with that data.
instance
  ( Newtype a b
  , Namespace b
  ) => Markable (List sep b a) where
  markTriviaTree bs t =
    mconcat
    $ map
      ( TriviaTree mempty . flip M.singleton t . SomeNamespace . (pack :: a -> b)
      )
      (_getList bs)

  unmarkTriviaTree bs t =
    fromMaybe mempty
      $ mconcat
      $ map
        (flip M.lookup (namedAnnotations t) . SomeNamespace . (pack :: a -> b))
        (_getList bs)

-- TODO(leana8959): we are forced to define this and we don't use it
-- It is due to the ExactPretty constraint.
instance
  ( Newtype a b
  , ExactPretty b
  , TriviaSep sep
  , Namespace b
  , Markable b
  )
  => ExactPretty (List sep b a) where
  exactPretty t0 n =
    let
        docGroups :: [[(TriviaTree, b)]] =
              groupBy ((==) `on` (fromMaybe 0 . atFieldNth . justAnnotation . fst))
              $ sortOn (fromMaybe 0 . atFieldNth . justAnnotation . fst)
              $ map
                ( \o ->
                    let n = (pack :: a -> b) o -- pack each element
                        tChildren = unmarkTriviaTree n t0
                    in
                          ( tChildren
                          , n
                          )
                )
              $ unpack -- unpack the list
              $ n
     in
        vcat
        $ map (prettierSep (Proxy :: Proxy sep))
        $ docGroups

instance
  ( Newtype a b
  , Sep sep
  , TriviaSep sep
  , ExactPretty b
  , Namespace b
  ) => ExactPrettyField (List sep b a) where
  exactPrettyField fieldName t0 list =
    let docGroups :: [[(TriviaTree, b)]] =
              groupBy
                ( (==) `on` (fromMaybe 0 . atFieldNth . justAnnotation . fst)
                )
              $ sortOn
                ( fromMaybe 0 . atFieldNth . justAnnotation . fst
                )
              $ map
                ( \old ->
                    let new = (pack :: a -> b) old
                        -- The numbering are associated under the newtype
                        -- Move them upwards so we can sort
                        numbering = justAnnotation (unmarkTriviaTree new t0)
                        t = TriviaTree numbering mempty <> t0
                    in  (t, new)
                )
                (_getList list)
     in
          map
            ( PrettyField Nothing fieldName . prettierSep (Proxy :: Proxy sep)
            )
            docGroups


-- | Like 'List', but for 'Set'.
--
-- @since 3.2.0.0
newtype Set' sep b a = Set' {_getSet :: Set a}

-- | 'alaSet' and 'alaSet'' are simply 'Set'' constructor, with additional phantom
-- arguments to constrain the resulting type
--
-- >>> :t alaSet VCat
-- alaSet VCat :: Set a -> Set' VCat (Identity a) a
--
-- >>> :t alaSet' FSep Token
-- alaSet' FSep Token :: Set String -> Set' FSep Token String
--
-- >>> unpack' (alaSet' FSep Token) <$> eitherParsec "foo bar foo"
-- Right (fromList ["bar","foo"])
--
-- @since 3.2.0.0
alaSet :: sep -> Set a -> Set' sep (Identity a) a
alaSet _ = Set'

-- | More general version of 'alaSet'.
--
-- @since 3.2.0.0
alaSet' :: sep -> (a -> b) -> Set a -> Set' sep b a
alaSet' _ _ = Set'

instance Newtype (Set a) (Set' sep wrapper a)

instance (Newtype a b, Ord a, Sep sep, Parsec b) => Parsec (Set' sep b a) where
  parsec = pack . Set.fromList . map (unpack :: b -> a) <$> parseSep (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep sep, Pretty b) => Pretty (Set' sep b a) where
  pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (pack :: a -> b)) . Set.toList . unpack

--

-- | Like 'List', but for 'NonEmpty'.
--
-- @since 3.2.0.0
newtype NonEmpty' sep b a = NonEmpty' {_getNonEmpty :: NonEmpty a}

-- | 'alaNonEmpty' and 'alaNonEmpty'' are simply 'NonEmpty'' constructor, with additional phantom
-- arguments to constrain the resulting type
--
-- >>> :t alaNonEmpty VCat
-- alaNonEmpty VCat :: NonEmpty a -> NonEmpty' VCat (Identity a) a
--
-- >>> unpack' (alaNonEmpty' FSep Token) <$> eitherParsec "foo bar foo"
-- Right ("foo" :| ["bar","foo"])
--
-- @since 3.2.0.0
alaNonEmpty :: sep -> NonEmpty a -> NonEmpty' sep (Identity a) a
alaNonEmpty _ = NonEmpty'

-- | More general version of 'alaNonEmpty'.
--
-- @since 3.2.0.0
alaNonEmpty' :: sep -> (a -> b) -> NonEmpty a -> NonEmpty' sep b a
alaNonEmpty' _ _ = NonEmpty'

instance Newtype (NonEmpty a) (NonEmpty' sep wrapper a)

instance (Newtype a b, Sep sep, Parsec b) => Parsec (NonEmpty' sep b a) where
  parsec = pack . fmap (unpack :: b -> a) <$> parseSepNE (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep sep, Pretty b) => Pretty (NonEmpty' sep b a) where
  pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (pack :: a -> b)) . NE.toList . unpack

-------------------------------------------------------------------------------
-- Identifiers
-------------------------------------------------------------------------------

-- | Haskell string or @[^ ,]+@
newtype Token = Token {getToken :: String}
  deriving (Eq, Ord, Show)

instance Newtype String Token

instance Parsec Token where parsec = snd <$> exactParsec
instance ExactParsec Token where
  exactParsec = (mempty,) . pack <$> parsecToken

instance Pretty Token where
  pretty = showToken . unpack

instance Markable Token
instance ExactPretty Token where exactPretty _ = pretty

-- | Haskell string or @[^ ]+@
newtype Token' = Token' {getToken' :: String}
  deriving (Eq, Show, Ord)

instance Newtype String Token'

instance Parsec Token' where parsec = snd <$> exactParsec
instance ExactParsec Token' where
  exactParsec = (mempty,) . pack <$> parsecToken'

instance Pretty Token' where
  pretty = showToken . unpack

instance Markable Token'
instance ExactPretty Token' where exactPretty _ = pretty

-- | Either @"quoted"@ or @un-quoted@.
newtype MQuoted a = MQuoted {getMQuoted :: a}
  deriving (Eq, Ord, Show)

instance Newtype a (MQuoted a)

instance (Parsec a, Namespace a) => ExactParsec (MQuoted a) where exactParsec = (mempty,) <$> parsec
instance Parsec a => Parsec (MQuoted a) where
  parsec = pack <$> parsecMaybeQuoted parsec

instance Pretty a => Pretty (MQuoted a) where
  pretty = pretty . unpack

instance (Markable a, Namespace a) => Markable (MQuoted a)
instance (Namespace a, ExactPretty a) => ExactPretty (MQuoted a) where
  exactPretty t = exactPretty t . unpack

-- | Filepath are parsed as 'Token'.
newtype FilePathNT = FilePathNT {getFilePathNT :: String}
  deriving (Ord, Eq, Show)

instance Markable FilePathNT
instance ExactPretty FilePathNT where exactPretty _ = pretty

instance Newtype String FilePathNT

instance Parsec FilePathNT where parsec = snd <$> exactParsec
instance ExactParsec FilePathNT where
  exactParsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else return (mempty, FilePathNT token)

instance Pretty FilePathNT where
  pretty = showFilePath . unpack

-- | Newtype for 'SymbolicPath', with a different 'Parsec' instance
-- to disallow empty paths.
newtype SymbolicPathNT from to = SymbolicPathNT {getSymbolicPathNT :: SymbolicPath from to}
  deriving (Ord, Eq, Show)

instance
  ( Typeable from
  , Typeable to
  ) => Markable (SymbolicPathNT from to)

instance Newtype (SymbolicPath from to) (SymbolicPathNT from to)

-- TODO(leana8959): reversed
instance
  ( Typeable from
  , Typeable to
  ) => ExactParsec (SymbolicPathNT from to) where exactParsec = (mempty,) <$> parsec
instance Parsec (SymbolicPathNT from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else return (SymbolicPathNT $ makeSymbolicPath token)

instance Pretty (SymbolicPathNT from to) where
  pretty = showFilePath . getSymbolicPath . getSymbolicPathNT

instance (Typeable from, Typeable to) => ExactPretty (SymbolicPathNT from to) where
  exactPretty _ = pretty

-- | Newtype for 'RelativePath', with a different 'Parsec' instance
-- to disallow empty paths but allow non-relative paths (which get rejected
-- later with a different error message, see 'Distribution.PackageDescription.Check.Paths.checkPath')
newtype RelativePathNT from to = RelativePathNT {getRelativePathNT :: RelativePath from to}
  deriving (Ord, Eq, Show)

instance
  ( Typeable from
  , Typeable to
  ) => Markable (RelativePathNT from to)

instance Newtype (RelativePath from to) (RelativePathNT from to)

-- NB: we don't reject non-relative paths here; we allow them here and reject
-- later (see 'Distribution.PackageDescription.Check.Paths.checkPath').
-- TODO(leana8959): reversed
instance
  ( Typeable from
  , Typeable to
  ) => ExactParsec (RelativePathNT from to) where exactParsec = (mempty,) <$> parsec
instance Parsec (RelativePathNT from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else return (RelativePathNT $ unsafeMakeSymbolicPath token)

instance Pretty (RelativePathNT from to) where
  pretty = showFilePath . getSymbolicPath . getRelativePathNT

instance (Typeable from, Typeable to) => ExactPretty (RelativePathNT from to) where
  exactPretty _ = pretty

-------------------------------------------------------------------------------
-- SpecVersion
-------------------------------------------------------------------------------

-- | Version range or just version, i.e. @cabal-version@ field.
--
-- There are few things to consider:
--
-- * Starting with 2.2 the cabal-version field should be the first field in the
--   file and only exact version is accepted. Therefore if we get e.g.
--   @>= 2.2@, we fail.
--   See <https://github.com/haskell/cabal/issues/4899>
--
-- We have this newtype, as writing Parsec and Pretty instances
-- for CabalSpecVersion would cause cycle in modules:
--     Version -> CabalSpecVersion -> Parsec -> ...
newtype SpecVersion = SpecVersion {getSpecVersion :: CabalSpecVersion}
  deriving (Eq, Show, Ord) -- instances needed for tests

instance Markable SpecVersion

instance Newtype CabalSpecVersion SpecVersion

-- TODO(leana8959): defined in reversed order
instance ExactParsec SpecVersion where exactParsec = (mempty,) <$> parsec
instance Parsec SpecVersion where
  parsec = do
    e <- parsecSpecVersion
    let ver :: Version
        ver = either id specVersionFromRange e

        digits :: [Int]
        digits = versionNumbers ver

    case cabalSpecFromVersionDigits digits of
      Nothing -> fail $ "Unknown cabal spec version specified: " ++ prettyShow ver
      Just csv -> do
        -- Check some warnings:
        case e of
          -- example:   cabal-version: 1.10
          -- should be  cabal-version: >=1.10
          Left _v
            | csv < CabalSpecV1_12 ->
                parsecWarning PWTSpecVersion $
                  concat
                    [ "With 1.10 or earlier, the 'cabal-version' field must use "
                    , "range syntax rather than a simple version number. Use "
                    , "'cabal-version: >= " ++ prettyShow ver ++ "'."
                    ]
          -- example:   cabal-version: >=1.12
          -- should be  cabal-version: 1.12
          Right _vr
            | csv >= CabalSpecV1_12 ->
                parsecWarning PWTSpecVersion $
                  concat
                    [ "Packages with 'cabal-version: 1.12' or later should specify a "
                    , "specific version of the Cabal spec of the form "
                    , "'cabal-version: x.y'. "
                    , "Use 'cabal-version: " ++ prettyShow ver ++ "'."
                    ]
          -- example:   cabal-version: >=1.10 && <1.12
          -- should be  cabal-version: >=1.10
          Right vr
            | csv < CabalSpecV1_12
            , not (simpleSpecVersionRangeSyntax vr) ->
                parsecWarning PWTSpecVersion $
                  concat
                    [ "It is recommended that the 'cabal-version' field only specify a "
                    , "version range of the form '>= x.y' for older cabal versions. Use "
                    , "'cabal-version: >= " ++ prettyShow ver ++ "'. "
                    , "Tools based on Cabal 1.10 and later will ignore upper bounds."
                    ]
          -- otherwise no warnings
          _ -> pure ()

        return (pack csv)
    where
      parsecSpecVersion = Left <$> parsec <|> Right <$> range

      range = do
        vr <- parsec
        if specVersionFromRange vr >= mkVersion [2, 1]
          then fail "cabal-version higher than 2.2 cannot be specified as a range. See https://github.com/haskell/cabal/issues/4899"
          else return vr

      specVersionFromRange :: VersionRange -> Version
      specVersionFromRange versionRange = case asVersionIntervals versionRange of
        [] -> version0
        VersionInterval (LowerBound version _) _ : _ -> version

      simpleSpecVersionRangeSyntax = cataVersionRange alg
        where
          alg (OrLaterVersionF _) = True
          alg _ = False

instance Pretty SpecVersion where
  pretty (SpecVersion csv)
    | csv >= CabalSpecV1_12 = text (showCabalSpecVersion csv)
    | otherwise = text ">=" <<>> text (showCabalSpecVersion csv)

-- TODO(leana8959): this should be implemented differently
instance ExactPretty SpecVersion where exactPretty _ = pretty

-------------------------------------------------------------------------------
-- SpecLicense
-------------------------------------------------------------------------------

-- | SPDX License expression or legacy license
newtype SpecLicense = SpecLicense {getSpecLicense :: Either SPDX.License License}
  deriving (Show, Ord, Eq)

instance Markable SpecLicense

instance Newtype (Either SPDX.License License) SpecLicense

-- TODO(leana8959): defined in reverse
instance ExactParsec SpecLicense where exactParsec = (mempty,) <$> parsec
instance Parsec SpecLicense where
  parsec = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2
      then SpecLicense . Left <$> parsec
      else SpecLicense . Right <$> parsec

instance Pretty SpecLicense where
  pretty = either pretty pretty . unpack
instance ExactPretty SpecLicense where exactPretty _ = pretty

-------------------------------------------------------------------------------
-- TestedWith
-------------------------------------------------------------------------------

-- | Version range or just version
newtype TestedWith = TestedWith {getTestedWith :: (CompilerFlavor, VersionRange)}
  deriving (Ord, Eq, Show)

instance Newtype (CompilerFlavor, VersionRange) TestedWith

instance Markable TestedWith
instance Parsec TestedWith where parsec = snd <$> exactParsec
instance ExactParsec TestedWith where
  exactParsec = (mempty,) . pack <$> parsecTestedWith

instance Pretty TestedWith where
  pretty x = case unpack x of
    (compiler, vr) -> pretty compiler <+> pretty vr

instance ExactPretty TestedWith where exactPretty _ = pretty

parsecTestedWith :: CabalParsing m => m (CompilerFlavor, VersionRange)
parsecTestedWith = do
  name <- lexemeParsec
  ver <- parsec <|> pure anyVersion
  return (name, ver)
