{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.LibraryName.Internal
  ( LibraryName (..)
  , defaultLibName
  , maybeToLibraryName
  , libraryNameString
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.UnqualComponentName.Internal

import qualified Data.List.NonEmpty as NEL
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

import Distribution.Types.Namespace

data LibraryName
  = LMainLibName
  | LSubLibName UnqualComponentName
  deriving (Eq, Generic, Ord, Read, Show, Data)

instance Namespace LibraryName

instance Binary LibraryName
instance Structured LibraryName
instance NFData LibraryName where rnf = genericRnf

defaultLibName :: LibraryName
defaultLibName = LMainLibName

libraryNameString :: LibraryName -> Maybe UnqualComponentName
libraryNameString LMainLibName = Nothing
libraryNameString (LSubLibName n) = Just n

-- | Convert the 'UnqualComponentName' of a library into a
-- 'LibraryName'.
maybeToLibraryName :: Maybe UnqualComponentName -> LibraryName
maybeToLibraryName Nothing = LMainLibName
maybeToLibraryName (Just n) = LSubLibName n
