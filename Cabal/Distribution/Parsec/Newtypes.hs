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
    -- * Version & License
    SpecVersion (..),
    TestedWith (..),
    SpecLicense (..),
    -- * Identifiers
    Token (..),
    Token' (..),
    MQuoted (..),
    FreeText (..),
    FilePathNT (..),
    ) where

import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Prelude ()

import Data.Functor.Identity         (Identity (..))
import Data.List                     (dropWhileEnd)
import Distribution.CabalSpecVersion
import Distribution.Compiler         (CompilerFlavor)
import Distribution.License          (License)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Version
       (LowerBound (..), Version, VersionRange, anyVersion, asVersionIntervals, mkVersion)
import Text.PrettyPrint              (Doc, comma, fsep, punctuate, vcat, (<+>))

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

-- | Proxy, internal to this module.
data P sep = P

class    Sep sep  where
    prettySep :: P sep -> [Doc] -> Doc

    parseSep :: CabalParsing m => P sep -> m a -> m [a]

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
newtype List sep b a = List { getList :: [a] }

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

instance Newtype (List sep wrapper a) [a] where
    pack = List
    unpack = getList

instance (Newtype b a, Sep sep, Parsec b) => Parsec (List sep b a) where
    parsec   = pack . map (unpack :: b -> a) <$> parseSep (P :: P sep) parsec

instance (Newtype b a, Sep sep, Pretty b) => Pretty (List sep b a) where
    pretty = prettySep (P :: P sep) . map (pretty . (pack :: a -> b)) . unpack

-- | Haskell string or @[^ ,]+@
newtype Token = Token { getToken :: String }

instance Newtype Token String where
    pack = Token
    unpack = getToken

instance Parsec Token where
    parsec = pack <$> parsecToken

instance Pretty Token where
    pretty = showToken . unpack

-- | Haskell string or @[^ ]+@
newtype Token' = Token' { getToken' :: String }

instance Newtype Token' String where
    pack = Token'
    unpack = getToken'

instance Parsec Token' where
    parsec = pack <$> parsecToken'

instance Pretty Token' where
    pretty = showToken . unpack

-- | Either @"quoted"@ or @un-quoted@.
newtype MQuoted a = MQuoted { getMQuoted :: a }

instance Newtype (MQuoted a) a where
    pack = MQuoted
    unpack = getMQuoted

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

instance Newtype SpecVersion (Either Version VersionRange) where
    pack = SpecVersion
    unpack = getSpecVersion

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

instance Newtype SpecLicense (Either SPDX.License License) where
    pack = SpecLicense
    unpack = getSpecLicense

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

instance Newtype TestedWith (CompilerFlavor, VersionRange) where
    pack = TestedWith
    unpack = getTestedWith

instance Parsec TestedWith where
    parsec = pack <$> parsecTestedWith

instance Pretty TestedWith where
    pretty x = case unpack x of
        (compiler, vr) -> pretty compiler <+> pretty vr

-- | This is /almost/ @'many' 'Distribution.Compat.P.anyChar'@, but it
--
-- * trims whitespace from ends of the lines,
--
-- * converts lines with only single dot into empty line.
--
newtype FreeText = FreeText { getFreeText :: String }

instance Newtype FreeText String where
    pack = FreeText
    unpack = getFreeText

instance Parsec FreeText where
    parsec = pack . dropDotLines <$ P.spaces <*> many P.anyChar
      where
        -- Example package with dot lines
        -- http://hackage.haskell.org/package/copilot-cbmc-0.1/copilot-cbmc.cabal
        dropDotLines "." = "."
        dropDotLines x = intercalate "\n" . map dotToEmpty . lines $ x
        dotToEmpty x | trim' x == "." = ""
        dotToEmpty x                  = trim x

        trim' :: String -> String
        trim' = dropWhileEnd (`elem` (" \t" :: String))

        trim :: String -> String
        trim = dropWhile isSpace . dropWhileEnd isSpace

instance Pretty FreeText where
    pretty = showFreeText . unpack

-- | Filepath are parsed as 'Token'.
newtype FilePathNT = FilePathNT { getFilePathNT :: String }

instance Newtype FilePathNT String where
    pack = FilePathNT
    unpack = getFilePathNT

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
