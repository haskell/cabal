{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}
-- | This module provides @newtype@ wrappers to be used with "Distribution.FieldGrammar".
module Distribution.Parsec.Newtypes (
    -- * List
    alaList,
    alaList',
    -- ** Modifiers
    CommaVCat (..),
    CommaFSep (..),
    VCat (..),
    FSep (..),
    NoCommaFSep (..),
    Sep (..),
    -- ** Type
    List,
    -- * Set
    alaSet,
    alaSet',
    Set',
    -- * Version & License
    SpecVersion (..),
    TestedWith (..),
    SpecLicense (..),
    -- * Identifiers
    Token (..),
    Token' (..),
    MQuoted (..),
    FilePathNT (..),
    ) where

import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Compiler         (CompilerFlavor)
import Distribution.License          (License)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Version          (LowerBound (..), Version, VersionRange, anyVersion, asVersionIntervals, mkVersion)
import Text.PrettyPrint              (Doc, comma, fsep, punctuate, vcat, (<+>))

import qualified Data.Set                        as Set
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.SPDX               as SPDX

-- | Vertical list with commas. Displayed with 'vcat'
data CommaVCat = CommaVCat

-- | Paragraph fill list with commas. Displayed with 'fsep'
data CommaFSep = CommaFSep

-- | Vertical list with optional commas. Displayed with 'vcat'.
data VCat = VCat

-- | Paragraph fill list with optional commas. Displayed with 'fsep'.
data FSep = FSep

-- | Paragraph fill list without commas. Displayed with 'fsep'.
data NoCommaFSep = NoCommaFSep

class    Sep sep  where
    prettySep :: Proxy sep -> [Doc] -> Doc

    parseSep :: CabalParsing m => Proxy sep -> m a -> m [a]

instance Sep CommaVCat where
    prettySep  _ = vcat . punctuate comma
    parseSep   _ p = do
        v <- askCabalSpecVersion
        if v >= CabalSpecV2_2 then parsecLeadingCommaList p else parsecCommaList p
instance Sep CommaFSep where
    prettySep _ = fsep . punctuate comma
    parseSep   _ p = do
        v <- askCabalSpecVersion
        if v >= CabalSpecV2_2 then parsecLeadingCommaList p else parsecCommaList p
instance Sep VCat where
    prettySep _  = vcat
    parseSep   _ p = do
        v <- askCabalSpecVersion
        if v >= CabalSpecV3_0 then parsecLeadingOptCommaList p else parsecOptCommaList p
instance Sep FSep where
    prettySep _  = fsep
    parseSep   _ p = do
        v <- askCabalSpecVersion
        if v >= CabalSpecV3_0 then parsecLeadingOptCommaList p else parsecOptCommaList p
instance Sep NoCommaFSep where
    prettySep _   = fsep
    parseSep  _ p = many (p <* P.spaces)

-- | List separated with optional commas. Displayed with @sep@, arguments of
-- type @a@ are parsed and pretty-printed as @b@.
newtype List sep b a = List { _getList :: [a] }

-- | 'alaList' and 'alaList'' are simply 'List', with additional phantom
-- arguments to constraint the resulting type
--
-- >>> :t alaList VCat
-- alaList VCat :: [a] -> List VCat (Identity a) a
--
-- >>> :t alaList' FSep Token
-- alaList' FSep Token :: [String] -> List FSep Token String
--
alaList :: sep -> [a] -> List sep (Identity a) a
alaList _ = List

-- | More general version of 'alaList'.
alaList' :: sep -> (a -> b) -> [a] -> List sep b a
alaList' _ _ = List

instance Newtype [a] (List sep wrapper a)

instance (Newtype a b, Sep sep, Parsec b) => Parsec (List sep b a) where
    parsec   = pack . map (unpack :: b -> a) <$> parseSep (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep sep, Pretty b) => Pretty (List sep b a) where
    pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (pack :: a -> b)) . unpack

-- | Like 'List', but for 'Set'.
--
-- @since 3.2.0.0
newtype Set' sep b a = Set' { _getSet :: Set a }

-- | 'alaSet' and 'alaSet'' are simply 'Set'' constructor, with additional phantom
-- arguments to constraint the resulting type
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
    parsec   = pack . Set.fromList . map (unpack :: b -> a) <$> parseSep (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep sep, Pretty b) => Pretty (Set' sep b a) where
    pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (pack :: a -> b)) . Set.toList . unpack

-- | Haskell string or @[^ ,]+@
newtype Token = Token { getToken :: String }

instance Newtype String Token

instance Parsec Token where
    parsec = pack <$> parsecToken

instance Pretty Token where
    pretty = showToken . unpack

-- | Haskell string or @[^ ]+@
newtype Token' = Token' { getToken' :: String }

instance Newtype String Token'

instance Parsec Token' where
    parsec = pack <$> parsecToken'

instance Pretty Token' where
    pretty = showToken . unpack

-- | Either @"quoted"@ or @un-quoted@.
newtype MQuoted a = MQuoted { getMQuoted :: a }

instance Newtype a (MQuoted a)

instance Parsec a => Parsec (MQuoted a) where
    parsec = pack <$> parsecMaybeQuoted parsec

instance Pretty a => Pretty (MQuoted a)  where
    pretty = pretty . unpack

-- | Version range or just version, i.e. @cabal-version@ field.
--
-- There are few things to consider:
--
-- * Starting with 2.2 the cabal-version field should be the first field in the
--   file and only exact version is accepted. Therefore if we get e.g.
--   @>= 2.2@, we fail.
--   See <https://github.com/haskell/cabal/issues/4899>
--
newtype SpecVersion = SpecVersion { getSpecVersion :: Either Version VersionRange }

instance Newtype (Either Version VersionRange) SpecVersion

instance Parsec SpecVersion where
    parsec = pack <$> parsecSpecVersion
      where
        parsecSpecVersion = Left <$> parsec <|> Right <$> range
        range = do
            vr <- parsec
            if specVersionFromRange vr >= mkVersion [2,1]
            then fail "cabal-version higher than 2.2 cannot be specified as a range. See https://github.com/haskell/cabal/issues/4899"
            else return vr

instance Pretty SpecVersion where
    pretty = either pretty pretty . unpack

specVersionFromRange :: VersionRange -> Version
specVersionFromRange versionRange = case asVersionIntervals versionRange of
    []                            -> mkVersion [0]
    ((LowerBound version _, _):_) -> version

-- | SPDX License expression or legacy license
newtype SpecLicense = SpecLicense { getSpecLicense :: Either SPDX.License License }

instance Newtype (Either SPDX.License License) SpecLicense

instance Parsec SpecLicense where
    parsec = do
        v <- askCabalSpecVersion
        if v >= CabalSpecV2_2
        then SpecLicense . Left <$> parsec
        else SpecLicense . Right <$> parsec

instance Pretty SpecLicense where
    pretty = either pretty pretty . unpack

-- | Version range or just version
newtype TestedWith = TestedWith { getTestedWith :: (CompilerFlavor, VersionRange) }

instance Newtype (CompilerFlavor, VersionRange) TestedWith

instance Parsec TestedWith where
    parsec = pack <$> parsecTestedWith

instance Pretty TestedWith where
    pretty x = case unpack x of
        (compiler, vr) -> pretty compiler <+> pretty vr

-- | Filepath are parsed as 'Token'.
newtype FilePathNT = FilePathNT { getFilePathNT :: String }

instance Newtype String FilePathNT

instance Parsec FilePathNT where
    parsec = pack <$> parsecToken

instance Pretty FilePathNT where
    pretty = showFilePath . unpack

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

parsecTestedWith :: CabalParsing m => m (CompilerFlavor, VersionRange)
parsecTestedWith = do
    name <- lexemeParsec
    ver  <- parsec <|> pure anyVersion
    return (name, ver)
