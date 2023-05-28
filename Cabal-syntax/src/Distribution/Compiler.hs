{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Compiler
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This has an enumeration of the various compilers that Cabal knows about. It
-- also specifies the default compiler. Sadly you'll often see code that does
-- case analysis on this compiler flavour enumeration like:
--
-- > case compilerFlavor comp of
-- >   GHC -> GHC.getInstalledPackages verbosity packageDb progdb
--
-- Obviously it would be better to use the proper 'Compiler' abstraction
-- because that would keep all the compiler-specific code together.
-- Unfortunately we cannot make this change yet without breaking the
-- 'UserHooks' api, which would break all custom @Setup.hs@ files, so for the
-- moment we just have to live with this deficiency. If you're interested, see
-- ticket #57.
module Distribution.Compiler
  ( -- * Compiler flavor
    CompilerFlavor (..)
  , buildCompilerId
  , buildCompilerFlavor
  , defaultCompilerFlavor
  , classifyCompilerFlavor
  , knownCompilerFlavors

    -- * Per compiler flavor
  , PerCompilerFlavor (..)
  , perCompilerFlavorToList

    -- * Compiler id
  , CompilerId (..)

    -- * Compiler info
  , CompilerInfo (..)
  , unknownCompilerInfo
  , AbiTag (..)
  , abiTagString
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Language.Haskell.Extension

import Distribution.Version (Version, mkVersion', nullVersion)

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec (Parsec (..))
import Distribution.Pretty (Pretty (..), prettyShow)
import qualified System.Info (compilerName, compilerVersion)
import qualified Text.PrettyPrint as Disp

data CompilerFlavor
  = GHC
  | GHCJS
  | NHC
  | YHC
  | Hugs
  | HBC
  | Helium
  | JHC
  | LHC
  | UHC
  | Eta
  | HaskellSuite String -- string is the id of the actual compiler
  | OtherCompiler String
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary CompilerFlavor
instance Structured CompilerFlavor
instance NFData CompilerFlavor where rnf = genericRnf

knownCompilerFlavors :: [CompilerFlavor]
knownCompilerFlavors =
  [GHC, GHCJS, NHC, YHC, Hugs, HBC, Helium, JHC, LHC, UHC, Eta]

instance Pretty CompilerFlavor where
  pretty (OtherCompiler name) = Disp.text name
  pretty (HaskellSuite name) = Disp.text name
  pretty NHC = Disp.text "nhc98"
  pretty other = Disp.text (lowercase (show other))

instance Parsec CompilerFlavor where
  parsec = classifyCompilerFlavor <$> component
    where
      component = do
        cs <- P.munch1 isAlphaNum
        if all isDigit cs then fail "all digits compiler name" else return cs

classifyCompilerFlavor :: String -> CompilerFlavor
classifyCompilerFlavor s =
  fromMaybe (OtherCompiler s) $ lookup (lowercase s) compilerMap
  where
    compilerMap =
      [ (lowercase (prettyShow compiler), compiler)
      | compiler <- knownCompilerFlavors
      ]

buildCompilerFlavor :: CompilerFlavor
buildCompilerFlavor = classifyCompilerFlavor System.Info.compilerName

buildCompilerVersion :: Version
buildCompilerVersion = mkVersion' System.Info.compilerVersion

buildCompilerId :: CompilerId
buildCompilerId = CompilerId buildCompilerFlavor buildCompilerVersion

-- | The default compiler flavour to pick when compiling stuff. This defaults
-- to the compiler used to build the Cabal lib.
--
-- However if it's not a recognised compiler then it's 'Nothing' and the user
-- will have to specify which compiler they want.
defaultCompilerFlavor :: Maybe CompilerFlavor
defaultCompilerFlavor = case buildCompilerFlavor of
  OtherCompiler _ -> Nothing
  _ -> Just buildCompilerFlavor

-------------------------------------------------------------------------------
-- Per compiler data
-------------------------------------------------------------------------------

-- | 'PerCompilerFlavor' carries only info per GHC and GHCJS
--
-- Cabal parses only @ghc-options@ and @ghcjs-options@, others are omitted.
data PerCompilerFlavor v = PerCompilerFlavor v v
  deriving
    ( Generic
    , Show
    , Read
    , Eq
    , Ord
    , Typeable
    , Data
    , Functor
    , Foldable
    , Traversable
    )

instance Binary a => Binary (PerCompilerFlavor a)
instance Structured a => Structured (PerCompilerFlavor a)
instance NFData a => NFData (PerCompilerFlavor a)

perCompilerFlavorToList :: PerCompilerFlavor v -> [(CompilerFlavor, v)]
perCompilerFlavorToList (PerCompilerFlavor a b) = [(GHC, a), (GHCJS, b)]

instance Semigroup a => Semigroup (PerCompilerFlavor a) where
  PerCompilerFlavor a b <> PerCompilerFlavor a' b' =
    PerCompilerFlavor
      (a <> a')
      (b <> b')

instance (Semigroup a, Monoid a) => Monoid (PerCompilerFlavor a) where
  mempty = PerCompilerFlavor mempty mempty
  mappend = (<>)

-- ------------------------------------------------------------

-- * Compiler Id

-- ------------------------------------------------------------

data CompilerId = CompilerId CompilerFlavor Version
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

instance Binary CompilerId
instance Structured CompilerId
instance NFData CompilerId where rnf = genericRnf

instance Pretty CompilerId where
  pretty (CompilerId f v)
    | v == nullVersion = pretty f
    | otherwise = pretty f <<>> Disp.char '-' <<>> pretty v

instance Parsec CompilerId where
  parsec = do
    flavour <- parsec
    version <- (P.char '-' >> parsec) <|> return nullVersion
    return (CompilerId flavour version)

lowercase :: String -> String
lowercase = map toLower

-- ------------------------------------------------------------

-- * Compiler Info

-- ------------------------------------------------------------

-- | Compiler information used for resolving configurations. Some
--   fields can be set to Nothing to indicate that the information is
--   unknown.
data CompilerInfo = CompilerInfo
  { compilerInfoId :: CompilerId
  -- ^ Compiler flavour and version.
  , compilerInfoAbiTag :: AbiTag
  -- ^ Tag for distinguishing incompatible ABI's on the same
  -- architecture/os.
  , compilerInfoCompat :: Maybe [CompilerId]
  -- ^ Other implementations that this compiler claims to be
  -- compatible with, if known.
  , compilerInfoLanguages :: Maybe [Language]
  -- ^ Supported language standards, if known.
  , compilerInfoExtensions :: Maybe [Extension]
  -- ^ Supported extensions, if known.
  }
  deriving (Generic, Show, Read)

instance Binary CompilerInfo

data AbiTag
  = NoAbiTag
  | AbiTag String
  deriving (Eq, Generic, Show, Read, Typeable)

instance Binary AbiTag
instance Structured AbiTag

instance Pretty AbiTag where
  pretty NoAbiTag = Disp.empty
  pretty (AbiTag tag) = Disp.text tag

instance Parsec AbiTag where
  parsec = do
    tag <- P.munch (\c -> isAlphaNum c || c == '_')
    if null tag then return NoAbiTag else return (AbiTag tag)

abiTagString :: AbiTag -> String
abiTagString NoAbiTag = ""
abiTagString (AbiTag tag) = tag

-- | Make a CompilerInfo of which only the known information is its CompilerId,
--   its AbiTag and that it does not claim to be compatible with other
--   compiler id's.
unknownCompilerInfo :: CompilerId -> AbiTag -> CompilerInfo
unknownCompilerInfo compilerId abiTag =
  CompilerInfo compilerId abiTag (Just []) Nothing Nothing
