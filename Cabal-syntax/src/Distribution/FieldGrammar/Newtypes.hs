{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
  , CommaVCatAnn (..)
  , CommaFSepAnn (..)
  , VCatAnn (..)
  , FSepAnn (..)
  , NoCommaFSepAnn (..)

    -- ** Separator class
  , Sep (..)

    -- ** Type
  , List
  , ListAnn
  , ListWith

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

import Distribution.CabalSpecVersion
import Distribution.Compiler (CompilerFlavor)
import Distribution.License (License)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Trivia
import Distribution.Types.Modify (Annotate)
import qualified Distribution.Types.Modify as Mod
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

import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.SPDX as SPDX

-- | Vertical list with commas. Displayed with 'vcat'
data CommaVCat = CommaVCat

data CommaVCatAnn = CommaVCatAnn

-- | Paragraph fill list with commas. Displayed with 'fsep'
data CommaFSep = CommaFSep

data CommaFSepAnn = CommaFSepAnn

-- | Vertical list with optional commas. Displayed with 'vcat'.
data VCat = VCat

data VCatAnn = VCatAnn

-- | Paragraph fill list with optional commas. Displayed with 'fsep'.
data FSep = FSep

data FSepAnn = FSepAnn

-- | Paragraph fill list without commas. Displayed with 'fsep'.
data NoCommaFSep = NoCommaFSep

data NoCommaFSepAnn = NoCommaFSepAnn

-- TODO(leana8959): Relax Sep to return a list of annotated docs with position
-- Use the position propagated back from applyTriviaDoc
class Sep (mod :: Mod.HasAnnotation) sep | sep -> mod where
  prettySep :: Proxy sep -> [Annotate mod Doc] -> Doc

  parseSep :: CabalParsing m => Proxy sep -> m a -> m [Annotate mod a]
  parseSepNE :: CabalParsing m => Proxy sep -> m a -> m (NonEmpty (Annotate mod a))

instance Sep Mod.HasNoAnn CommaVCat where
  prettySep _ = vcat . punctuate comma
  parseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then parsecLeadingCommaList p else parsecCommaList p
  parseSepNE _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then parsecLeadingCommaNonEmpty p else parsecCommaNonEmpty p

instance Sep Mod.HasAnn CommaVCatAnn where
  prettySep _ = mconcat . map (\(Ann t doc) -> applyTriviaDoc t doc)
  parseSep _ p = do
    v <- askCabalSpecVersion
    let p' = Ann mempty <$> p
    if v >= CabalSpecV2_2 then parsecLeadingCommaListAnn p' else parsecCommaListAnn p'
  parseSepNE _ p = do
    v <- askCabalSpecVersion
    let p' = Ann mempty <$> p
    if v >= CabalSpecV2_2 then parsecLeadingCommaNonEmptyAnn p' else parsecCommaNonEmptyAnn p'

instance Sep Mod.HasNoAnn CommaFSep where
  prettySep _ = fsep . punctuate comma
  parseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then parsecLeadingCommaList p else parsecCommaList p
  parseSepNE _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2 then parsecLeadingCommaNonEmpty p else parsecCommaNonEmpty p

instance Sep Mod.HasAnn CommaFSepAnn where
  prettySep _ = mconcat . map (\(Ann t doc) -> applyTriviaDoc t doc)
  parseSep _ p = do
    v <- askCabalSpecVersion
    let p' = Ann mempty <$> p
    if v >= CabalSpecV2_2 then parsecLeadingCommaListAnn p' else parsecCommaListAnn p'
  parseSepNE _ p = do
    v <- askCabalSpecVersion
    let p' = Ann mempty <$> p
    if v >= CabalSpecV2_2 then parsecLeadingCommaNonEmptyAnn p' else parsecCommaNonEmptyAnn p'

instance Sep Mod.HasNoAnn VCat where
  prettySep _ = vcat
  parseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV3_0 then parsecLeadingOptCommaList p else parsecOptCommaList p
  parseSepNE _ p = NE.some1 (p <* P.spaces)

instance Sep Mod.HasAnn VCatAnn where
  prettySep _ = mconcat . map (\(Ann t doc) -> applyTriviaDoc t doc)
  parseSep _ p = do
    v <- askCabalSpecVersion
    let p' = Ann mempty <$> p
    if v >= CabalSpecV3_0 then parsecLeadingOptCommaListAnn p' else parsecOptCommaListAnn p'
  parseSepNE _ p =
    NE.some1
      ( do
          x <- p
          post <- P.spaces'
          pure (Ann (postTrivia post) x)
      )

instance Sep Mod.HasNoAnn FSep where
  prettySep _ = fsep
  parseSep _ p = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV3_0 then parsecLeadingOptCommaList p else parsecOptCommaList p
  parseSepNE _ p = NE.some1 (p <* P.spaces)

instance Sep Mod.HasAnn FSepAnn where
  prettySep _ = mconcat . map (\(Ann t doc) -> applyTriviaDoc t doc)
  parseSep _ p = do
    v <- askCabalSpecVersion
    let p' = Ann mempty <$> p
    if v >= CabalSpecV3_0 then parsecLeadingOptCommaListAnn p' else parsecOptCommaListAnn p'
  parseSepNE _ p =
    NE.some1
      ( do
          x <- p
          post <- P.spaces'
          pure (Ann (postTrivia post) x)
      )

instance Sep Mod.HasNoAnn NoCommaFSep where
  prettySep _ = fsep
  parseSep _ p = many (p <* P.spaces)
  parseSepNE _ p = NE.some1 (p <* P.spaces)

instance Sep Mod.HasAnn NoCommaFSepAnn where
  prettySep _ = mconcat . map (\(Ann t doc) -> applyTriviaDoc t doc)
  parseSep _ p = many $ do
    x <- p
    post <- P.spaces'
    pure (Ann (postTrivia post) x)
  parseSepNE _ p = NE.some1 $ do
    x <- p
    post <- P.spaces'
    pure (Ann (postTrivia post) x)

-- | List separated with optional commas. Displayed with @sep@, arguments of
-- type @a@ are parsed and pretty-printed as @b@.
newtype ListWith mod sep b a = List {_getList :: [Annotate mod a]}

type List = ListWith Mod.HasNoAnn
type ListAnn = ListWith Mod.HasAnn

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
instance Newtype [Ann SurroundingText a] (ListAnn sep wrapper a)

instance (Newtype a b, Sep Mod.HasNoAnn sep, Parsec b) => Parsec (List sep b a) where
  parsec = pack . map (unpack :: b -> a) <$> parseSep (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep Mod.HasAnn sep, Parsec b) => Parsec (ListAnn sep b a) where
  parsec = pack . (map . fmap) (unpack :: b -> a) <$> parseSep (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep Mod.HasNoAnn sep, Pretty b) => Pretty (List sep b a) where
  pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (pack :: a -> b)) . unpack

instance (Newtype a b, Sep Mod.HasAnn sep, Pretty b) => Pretty (ListAnn sep b a) where
  pretty = prettySep (Proxy :: Proxy sep) . (map . fmap) (pretty . (pack :: a -> b)) . unpack

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

instance (Newtype a b, Ord a, Sep Mod.HasNoAnn sep, Parsec b) => Parsec (Set' sep b a) where
  parsec = pack . Set.fromList . map (unpack :: b -> a) <$> parseSep (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep Mod.HasNoAnn sep, Pretty b) => Pretty (Set' sep b a) where
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

instance (Newtype a b, Sep Mod.HasNoAnn sep, Parsec b) => Parsec (NonEmpty' sep b a) where
  parsec = pack . fmap (unpack :: b -> a) <$> parseSepNE (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep Mod.HasNoAnn sep, Pretty b) => Pretty (NonEmpty' sep b a) where
  pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (pack :: a -> b)) . NE.toList . unpack

-------------------------------------------------------------------------------
-- Identifiers
-------------------------------------------------------------------------------

-- | Haskell string or @[^ ,]+@
newtype Token = Token {getToken :: String}

instance Newtype String Token

instance Parsec Token where
  parsec = pack <$> parsecToken

instance Pretty Token where
  pretty = showToken . unpack

-- | Haskell string or @[^ ]+@
newtype Token' = Token' {getToken' :: String}

instance Newtype String Token'

instance Parsec Token' where
  parsec = pack <$> parsecToken'

instance Pretty Token' where
  pretty = showToken . unpack

-- | Either @"quoted"@ or @un-quoted@.
newtype MQuoted a = MQuoted {getMQuoted :: a}

instance Newtype a (MQuoted a)

instance Parsec a => Parsec (MQuoted a) where
  parsec = pack <$> parsecMaybeQuoted parsec

instance Pretty a => Pretty (MQuoted a) where
  pretty = pretty . unpack

-- | Filepath are parsed as 'Token'.
newtype FilePathNT = FilePathNT {getFilePathNT :: String}

instance Newtype String FilePathNT

instance Parsec FilePathNT where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else return (FilePathNT token)

instance Pretty FilePathNT where
  pretty = showFilePath . unpack

-- | Newtype for 'SymbolicPath', with a different 'Parsec' instance
-- to disallow empty paths.
newtype SymbolicPathNT from to = SymbolicPathNT {getSymbolicPathNT :: SymbolicPath from to}

instance Newtype (SymbolicPath from to) (SymbolicPathNT from to)

instance Parsec (SymbolicPathNT from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else return (SymbolicPathNT $ makeSymbolicPath token)

instance Pretty (SymbolicPathNT from to) where
  pretty = showFilePath . getSymbolicPath . getSymbolicPathNT

-- | Newtype for 'RelativePath', with a different 'Parsec' instance
-- to disallow empty paths but allow non-relative paths (which get rejected
-- later with a different error message, see 'Distribution.PackageDescription.Check.Paths.checkPath')
newtype RelativePathNT from to = RelativePathNT {getRelativePathNT :: RelativePath from to}

instance Newtype (RelativePath from to) (RelativePathNT from to)

-- NB: we don't reject non-relative paths here; we allow them here and reject
-- later (see 'Distribution.PackageDescription.Check.Paths.checkPath').
instance Parsec (RelativePathNT from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else return (RelativePathNT $ unsafeMakeSymbolicPath token)

instance Pretty (RelativePathNT from to) where
  pretty = showFilePath . getSymbolicPath . getRelativePathNT

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
  deriving (Eq, Show) -- instances needed for tests

instance Newtype CabalSpecVersion SpecVersion

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

-------------------------------------------------------------------------------
-- SpecLicense
-------------------------------------------------------------------------------

-- | SPDX License expression or legacy license
newtype SpecLicense = SpecLicense {getSpecLicense :: Either SPDX.License License}
  deriving (Show, Eq)

instance Newtype (Either SPDX.License License) SpecLicense

instance Parsec SpecLicense where
  parsec = do
    v <- askCabalSpecVersion
    if v >= CabalSpecV2_2
      then SpecLicense . Left <$> parsec
      else SpecLicense . Right <$> parsec

instance Pretty SpecLicense where
  pretty = either pretty pretty . unpack

-------------------------------------------------------------------------------
-- TestedWith
-------------------------------------------------------------------------------

-- | Version range or just version
newtype TestedWith = TestedWith {getTestedWith :: (CompilerFlavor, VersionRange)}

instance Newtype (CompilerFlavor, VersionRange) TestedWith

instance Parsec TestedWith where
  parsec = pack <$> parsecTestedWith

instance Pretty TestedWith where
  pretty x = case unpack x of
    (compiler, vr) -> pretty compiler <+> pretty vr

parsecTestedWith :: CabalParsing m => m (CompilerFlavor, VersionRange)
parsecTestedWith = do
  name <- lexemeParsec
  ver <- parsec <|> pure anyVersion
  return (name, ver)
