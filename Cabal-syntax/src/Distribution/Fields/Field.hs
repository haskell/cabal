{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Cabal-like file AST types: 'Field', 'Section' etc
--
-- These types are parametrized by an annotation.
module Distribution.Fields.Field
  ( -- * Cabal file
    Field (..)
  , fieldName
  , fieldAnn
  , fieldUniverse
  , FieldLine (..)
  , fieldLineAnn
  , fieldLineBS
  , SectionArg (..)
  , sectionArgAnn

    -- * Name
  , FieldName
  , Name (..)
  , mkName
  , getName
  , nameAnn

    -- * Conversions to String
  , sectionArgsToString
  , fieldLinesToString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char
import Distribution.Compat.Prelude
import Distribution.Pretty (showTokenStr)
import Distribution.Utils.Generic (fromUTF8BS)
import Prelude ()
#if MIN_VERSION_base(4,18,0)
import qualified Data.Foldable1 as F1
#endif

-------------------------------------------------------------------------------
-- Cabal file
-------------------------------------------------------------------------------

-- | A Cabal-like file consists of a series of fields (@foo: bar@) and sections (@library ...@).
data Field ann
  = Field !(Name ann) [FieldLine ann]
  | Section !(Name ann) [SectionArg ann] [Field ann]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Section of field name
fieldName :: Field ann -> Name ann
fieldName (Field n _) = n
fieldName (Section n _ _) = n

fieldAnn :: Field ann -> ann
fieldAnn = nameAnn . fieldName

-- | All transitive descendants of 'Field', including itself.
--
-- /Note:/ the resulting list is never empty.
fieldUniverse :: Field ann -> [Field ann]
fieldUniverse f@(Section _ _ fs) = f : concatMap fieldUniverse fs
fieldUniverse f@(Field _ _) = [f]

-- | A line of text representing the value of a field from a Cabal file.
-- A field may contain multiple lines.
--
-- /Invariant:/ 'ByteString' has no newlines.
data FieldLine ann = FieldLine !ann !ByteString
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | @since 3.0.0.0
fieldLineAnn :: FieldLine ann -> ann
fieldLineAnn (FieldLine ann _) = ann

-- | @since 3.0.0.0
fieldLineBS :: FieldLine ann -> ByteString
fieldLineBS (FieldLine _ bs) = bs

-- | Section arguments, e.g. name of the library
data SectionArg ann
  = -- | identifier, or something which looks like number. Also many dot numbers, i.e. "7.6.3"
    SecArgName !ann !ByteString
  | -- | quoted string
    SecArgStr !ann !ByteString
  | -- | everything else, mm. operators (e.g. in if-section conditionals)
    SecArgOther !ann !ByteString
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Extract annotation from 'SectionArg'.
sectionArgAnn :: SectionArg ann -> ann
sectionArgAnn (SecArgName ann _) = ann
sectionArgAnn (SecArgStr ann _) = ann
sectionArgAnn (SecArgOther ann _) = ann

-------------------------------------------------------------------------------
-- Name
-------------------------------------------------------------------------------

type FieldName = ByteString

-- | A field name.
--
-- /Invariant/: 'ByteString' is lower-case ASCII.
data Name ann = Name !ann !FieldName
  deriving (Eq, Show, Functor, Foldable, Traversable)

mkName :: ann -> FieldName -> Name ann
mkName ann bs = Name ann (B.map Char.toLower bs)

getName :: Name ann -> FieldName
getName (Name _ bs) = bs

nameAnn :: Name ann -> ann
nameAnn (Name ann _) = ann

-------------------------------------------------------------------------------
-- To Strings
-------------------------------------------------------------------------------

-- |
--
-- @since 3.6.0.0
sectionArgsToString :: [SectionArg ann] -> String
sectionArgsToString = unwords . map toStr
  where
    toStr :: SectionArg ann -> String
    toStr (SecArgName _ bs) = showTokenStr (fromUTF8BS bs)
    toStr (SecArgStr _ bs) = showTokenStr (fromUTF8BS bs)
    toStr (SecArgOther _ bs) = fromUTF8BS bs

-- | Convert @['FieldLine']@ into String.
--
-- /Note:/ this doesn't preserve indentation or empty lines,
-- as the annotations (e.g. positions) are ignored.
--
-- @since 3.6.0.0
fieldLinesToString :: [FieldLine ann] -> String
fieldLinesToString =
  -- intercalate to avoid trailing newline.
  intercalate "\n" . map toStr
  where
    toStr (FieldLine _ bs) = fromUTF8BS bs

-------------------------------------------------------------------------------
-- Foldable1
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,18,0)

-- | @since 3.12.0.0
instance F1.Foldable1 Field where
  foldMap1 f (Field x ys) =
    F1.fold1 (F1.foldMap1 f x :| map (F1.foldMap1 f) ys)
  foldMap1 f (Section x ys zs) =
    F1.fold1 (F1.foldMap1 f x :| map (F1.foldMap1 f) ys ++ map (F1.foldMap1 f) zs)

-- | @since 3.12.0.0
instance F1.Foldable1 FieldLine where
  foldMap1 = (. fieldLineAnn)

-- | @since 3.12.0.0
instance F1.Foldable1 SectionArg where
  foldMap1 = (. sectionArgAnn)

-- | @since 3.12.0.0
instance F1.Foldable1 Name where
  foldMap1 = (. nameAnn)

#endif
