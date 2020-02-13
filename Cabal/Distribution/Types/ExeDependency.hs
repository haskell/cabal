{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.ExeDependency
  ( ExeDependency(..)
  , qualifiedExeName
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.FieldGrammar.Described
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.ComponentName
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Version                   (VersionRange, anyVersion)

import qualified Distribution.Compat.CharParsing as P
import           Text.PrettyPrint                (text, (<+>))

-- | Describes a dependency on an executable from a package
--
data ExeDependency = ExeDependency
                     PackageName
                     UnqualComponentName -- name of executable component of package
                     VersionRange
                     deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary ExeDependency
instance Structured ExeDependency
instance NFData ExeDependency where rnf = genericRnf

instance Pretty ExeDependency where
  pretty (ExeDependency name exe ver) =
    (pretty name <<>> text ":" <<>> pretty exe) <+> pretty ver

-- | 
--
-- Examples
--
-- >>> simpleParsec "happy:happy" :: Maybe ExeDependency
-- Just (ExeDependency (PackageName "happy") (UnqualComponentName "happy") AnyVersion)
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
--
instance Parsec ExeDependency where
    parsec = do
        name <- parsec
        _    <- P.char ':'
        exe  <- lexemeParsec
        ver  <- parsec <|> pure anyVersion
        return (ExeDependency name exe ver)

instance Described ExeDependency where
    describe _ = RETodo

qualifiedExeName :: ExeDependency -> ComponentName
qualifiedExeName (ExeDependency _ ucn _) = CExeName ucn

