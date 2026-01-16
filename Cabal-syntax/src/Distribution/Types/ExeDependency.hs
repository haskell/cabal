{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ExeDependency
  ( ExeDependency (..)
  , qualifiedExeName
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.ComponentName
import Distribution.Types.PackageName
import Distribution.Types.PackageName.Parsec
import Distribution.Types.UnqualComponentName
import Distribution.Version (VersionRange, anyVersion, isAnyVersion)

import Distribution.Types.Version.Parsec
import Distribution.Types.VersionRange.Parsec
import Distribution.Types.Namespace

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

-- | Describes a dependency on an executable from a package
data ExeDependency
  = ExeDependency
      PackageName
      UnqualComponentName -- name of executable component of package
      VersionRange
  deriving (Generic, Read, Show, Eq, Ord, Data)

instance Namespace ExeDependency

instance Binary ExeDependency
instance Structured ExeDependency
instance NFData ExeDependency where rnf = genericRnf

instance Pretty ExeDependency where
  pretty (ExeDependency name exe ver) =
    pretty name <<>> PP.colon <<>> pretty exe PP.<+> pver
    where
      pver
        | isAnyVersion ver = PP.empty
        | otherwise = pretty ver

-- TODO(leana8959): maybe important
instance Prettier ExeDependency where
  prettier _ = pretty

-- |
--
-- Examples
--
-- >>> simpleParsec "happy:happy" :: Maybe ExeDependency
-- Just (ExeDependency (PackageName "happy") (UnqualComponentName "happy") (OrLaterVersion (mkVersion [0])))
--
-- >>> simpleParsec "happy:happy >= 1.19.12" :: Maybe ExeDependency
-- Just (ExeDependency (PackageName "happy") (UnqualComponentName "happy") (OrLaterVersion (mkVersion [1,19,12])))
--
-- >>> simpleParsec "happy:happy>=1.19.12" :: Maybe ExeDependency
-- Just (ExeDependency (PackageName "happy") (UnqualComponentName "happy") (OrLaterVersion (mkVersion [1,19,12])))
--
-- >>> simpleParsec "happy : happy >= 1.19.12" :: Maybe ExeDependency
-- Nothing
--
-- >>> simpleParsec "happy: happy >= 1.19.12" :: Maybe ExeDependency
-- Nothing
--
-- >>> simpleParsec "happy :happy >= 1.19.12" :: Maybe ExeDependency
-- Nothing
instance Parsec ExeDependency where
  parsec = do
    name <- parsec
    _ <- P.char ':'
    exe <- lexemeParsec
    ver <- parsec <|> pure anyVersion
    return (ExeDependency name exe ver)

qualifiedExeName :: ExeDependency -> ComponentName
qualifiedExeName (ExeDependency _ ucn _) = CExeName ucn
