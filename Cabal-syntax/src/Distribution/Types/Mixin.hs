{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Mixin
  ( Mixin (..)
  , mkMixin
  , normaliseMixin
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.IncludeRenaming
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

-- |
--
-- /Invariant:/ if 'mixinLibraryName' is 'LSubLibName', it's not
-- the same as 'mixinPackageName'. In other words,
-- the same invariant as 'Dependency' has.
data Mixin = Mixin
  { mixinPackageName :: PackageName
  , mixinLibraryName :: LibraryName
  , mixinIncludeRenaming :: IncludeRenaming
  }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance Binary Mixin
instance Structured Mixin

instance NFData Mixin where rnf = genericRnf

instance Pretty Mixin where
  pretty (Mixin pn LMainLibName incl) = pretty pn <+> pretty incl
  pretty (Mixin pn (LSubLibName ln) incl) = pretty pn <<>> PP.colon <<>> pretty ln <+> pretty incl

-- |
--
-- >>>  simpleParsec "mylib" :: Maybe Mixin
-- Just (Mixin {mixinPackageName = PackageName "mylib", mixinLibraryName = LMainLibName, mixinIncludeRenaming = IncludeRenaming {includeProvidesRn = DefaultRenaming, includeRequiresRn = DefaultRenaming}})
--
-- >>>  simpleParsec "thatlib:sublib" :: Maybe Mixin
-- Just (Mixin {mixinPackageName = PackageName "thatlib", mixinLibraryName = LSubLibName (UnqualComponentName "sublib"), mixinIncludeRenaming = IncludeRenaming {includeProvidesRn = DefaultRenaming, includeRequiresRn = DefaultRenaming}})
--
-- >>>  simpleParsec "thatlib:thatlib" :: Maybe Mixin
-- Just (Mixin {mixinPackageName = PackageName "thatlib", mixinLibraryName = LMainLibName, mixinIncludeRenaming = IncludeRenaming {includeProvidesRn = DefaultRenaming, includeRequiresRn = DefaultRenaming}})
--
-- Sublibrary syntax is accepted since @cabal-version: 3.4@.
--
-- >>> map (`simpleParsec'` "mylib:sub") [CabalSpecV3_0, CabalSpecV3_4] :: [Maybe Mixin]
-- [Nothing,Just (Mixin {mixinPackageName = PackageName "mylib", mixinLibraryName = LSubLibName (UnqualComponentName "sub"), mixinIncludeRenaming = IncludeRenaming {includeProvidesRn = DefaultRenaming, includeRequiresRn = DefaultRenaming}})]
instance Parsec Mixin where
  parsec = do
    pn <- parsec
    ln <- P.option LMainLibName $ do
      _ <- P.char ':'
      versionGuardMultilibs
      LSubLibName <$> parsec
    P.spaces
    incl <- parsec
    return (mkMixin pn ln incl)

versionGuardMultilibs :: CabalParsing m => m ()
versionGuardMultilibs = do
  csv <- askCabalSpecVersion
  when (csv < CabalSpecV3_4) $
    fail $
      unwords
        [ "Sublibrary mixin syntax used."
        , "To use this syntax the package needs to specify at least 'cabal-version: 3.4'."
        ]

-- | Smart constructor of 'Mixin', enforces invariant.
--
-- @since 3.4.0.0
mkMixin :: PackageName -> LibraryName -> IncludeRenaming -> Mixin
mkMixin pn (LSubLibName uqn) incl
  | packageNameToUnqualComponentName pn == uqn =
      Mixin pn LMainLibName incl
mkMixin pn ln incl =
  Mixin pn ln incl

-- | Restore invariant
normaliseMixin :: Mixin -> Mixin
normaliseMixin (Mixin pn ln incl) = mkMixin pn ln incl
