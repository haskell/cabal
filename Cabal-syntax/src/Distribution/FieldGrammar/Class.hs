{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Distribution.FieldGrammar.Class
  ( FieldGrammar (..)
  , uniqueField
  , optionalField
  , optionalFieldDef
  , monoidalField
  , defaultFreeTextFieldDefST
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Compat.Newtype (Newtype)
import Distribution.FieldGrammar.Newtypes
import Distribution.Fields.Field
import Distribution.Utils.ShortText

-- | 'FieldGrammar' is parametrised by
--
-- * @s@ which is a structure we are parsing. We need this to provide prettyprinter
-- functionality
--
-- * @a@ type of the field.
--
-- /Note:/ We'd like to have @forall s. Applicative (f s)@ context.
class
  ( c SpecVersion
  , c TestedWith
  , c SpecLicense
  , c Token
  , c Token'
  , c FilePathNT
  ) =>
  FieldGrammar c g
    | g -> c
  where
  -- | Unfocus, zoom out, /blur/ 'FieldGrammar'.
  blurFieldGrammar :: ALens' a b -> g b d -> g a d

  -- | Field which should be defined, exactly once.
  uniqueFieldAla
    :: (c b, Newtype a b)
    => FieldName
    -- ^ field name
    -> (a -> b)
    -- ^ 'Newtype' pack
    -> ALens' s a
    -- ^ lens into the field
    -> g s a

  -- | Boolean field with a default value.
  booleanFieldDef
    :: FieldName
    -- ^ field name
    -> ALens' s Bool
    -- ^ lens into the field
    -> Bool
    -- ^ default
    -> g s Bool

  -- | Optional field.
  optionalFieldAla
    :: (c b, Newtype a b)
    => FieldName
    -- ^ field name
    -> (a -> b)
    -- ^ 'pack'
    -> ALens' s (Maybe a)
    -- ^ lens into the field
    -> g s (Maybe a)

  -- | Optional field with default value.
  optionalFieldDefAla
    :: (c b, Newtype a b, Eq a)
    => FieldName
    -- ^ field name
    -> (a -> b)
    -- ^ 'Newtype' pack
    -> ALens' s a
    -- ^ @'Lens'' s a@: lens into the field
    -> a
    -- ^ default value
    -> g s a

  --  | Free text field is essentially 'optionalFieldDefAla` with @""@
  --  as the default and "accept everything" parser.
  --
  -- @since 3.0.0.0
  freeTextField
    :: FieldName
    -> ALens' s (Maybe String)
    -- ^ lens into the field
    -> g s (Maybe String)

  --  | Free text field is essentially 'optionalFieldDefAla` with @""@
  --  as the default and "accept everything" parser.
  --
  -- @since 3.0.0.0
  freeTextFieldDef
    :: FieldName
    -> ALens' s String
    -- ^ lens into the field
    -> g s String

  -- | @since 3.2.0.0
  freeTextFieldDefST
    :: FieldName
    -> ALens' s ShortText
    -- ^ lens into the field
    -> g s ShortText

  -- | Monoidal field.
  --
  -- Values are combined with 'mappend'.
  --
  -- /Note:/ 'optionalFieldAla' is a @monoidalField@ with 'Last' monoid.
  monoidalFieldAla
    :: (c b, Monoid a, Newtype a b)
    => FieldName
    -- ^ field name
    -> (a -> b)
    -- ^ 'pack'
    -> ALens' s a
    -- ^ lens into the field
    -> g s a

  -- | Parser matching all fields with a name starting with a prefix.
  prefixedFields
    :: FieldName
    -- ^ field name prefix
    -> ALens' s [(String, String)]
    -- ^ lens into the field
    -> g s [(String, String)]

  -- | Known field, which we don't parse, nor pretty print.
  knownField :: FieldName -> g s ()

  -- | Field which is parsed but not pretty printed.
  hiddenField :: g s a -> g s a

  -- | Deprecated since
  deprecatedSince
    :: CabalSpecVersion
    -- ^ version
    -> String
    -- ^ deprecation message
    -> g s a
    -> g s a

  -- | Removed in. If we encounter removed field, parsing fails.
  removedIn
    :: CabalSpecVersion
    -- ^ version
    -> String
    -- ^ removal message
    -> g s a
    -> g s a

  -- | Annotate field with since spec-version.
  availableSince
    :: CabalSpecVersion
    -- ^ spec version
    -> a
    -- ^ default value
    -> g s a
    -> g s a

  -- | Annotate field with since spec-version.
  -- This is used to recognise, but warn about the field.
  -- It is used to process @other-extensions@ field.
  --
  -- Default implementation is to not warn.
  --
  -- @since 3.4.0.0
  availableSinceWarn
    :: CabalSpecVersion
    -- ^ spec version
    -> g s a
    -> g s a
  availableSinceWarn _ = id

-- | Field which can be defined at most once.
uniqueField
  :: (FieldGrammar c g, c (Identity a))
  => FieldName
  -- ^ field name
  -> ALens' s a
  -- ^ lens into the field
  -> g s a
uniqueField fn l = uniqueFieldAla fn Identity l

-- | Field which can be defined at most once.
optionalField
  :: (FieldGrammar c g, c (Identity a))
  => FieldName
  -- ^ field name
  -> ALens' s (Maybe a)
  -- ^ lens into the field
  -> g s (Maybe a)
optionalField fn l = optionalFieldAla fn Identity l

-- | Optional field with default value.
optionalFieldDef
  :: (FieldGrammar c g, Functor (g s), c (Identity a), Eq a)
  => FieldName
  -- ^ field name
  -> ALens' s a
  -- ^ @'Lens'' s a@: lens into the field
  -> a
  -- ^ default value
  -> g s a
optionalFieldDef fn l x = optionalFieldDefAla fn Identity l x

-- | Field which can be define multiple times, and the results are @mappend@ed.
monoidalField
  :: (FieldGrammar c g, c (Identity a), Monoid a)
  => FieldName
  -- ^ field name
  -> ALens' s a
  -- ^ lens into the field
  -> g s a
monoidalField fn l = monoidalFieldAla fn Identity l

-- | Default implementation for 'freeTextFieldDefST'.
defaultFreeTextFieldDefST
  :: (Functor (g s), FieldGrammar c g)
  => FieldName
  -> ALens' s ShortText
  -- ^ lens into the field
  -> g s ShortText
defaultFreeTextFieldDefST fn l =
  toShortText <$> freeTextFieldDef fn (cloneLens l . st)
  where
    st :: Lens' ShortText String
    st f s = toShortText <$> f (fromShortText s)
