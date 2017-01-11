{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.ExeDependency
  ( ExeDependency(..)
  , qualifiedExeName
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Package
import Distribution.Types.ComponentName
import Distribution.Types.UnqualComponentName
import Distribution.Version ( VersionRange, anyVersion )

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP
import Distribution.Text

import Text.PrettyPrint ((<+>), text)

-- | Describes a dependency on an executable from a package
--
data ExeDependency = ExeDependency
                     PackageName
                     -- Name of specific executable component of package, or
                     -- nothing for a wilcard dependency on them all
                     (Maybe UnqualComponentName)
                     VersionRange
                     deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary ExeDependency
instance NFData ExeDependency where rnf = genericRnf

instance Text ExeDependency where
  disp (ExeDependency name exe ver) =
    (disp name <<>> text ":" <<>> exe') <+> disp ver
    where exe' = case exe of
            Just e  -> disp e
            Nothing -> text "*"

  parse = do name <- parse
             _ <- Parse.char ':'
             exe <- (Just <$> parse) <++ (Parse.char '*' >> pure Nothing)
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return (ExeDependency name exe ver)

qualifiedExeName :: ExeDependency -> Maybe ComponentName
qualifiedExeName (ExeDependency _ ucn _) = CExeName <$> ucn
