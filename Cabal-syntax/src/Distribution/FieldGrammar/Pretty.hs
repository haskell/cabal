{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.FieldGrammar.Pretty
  ( PrettyFieldGrammar
  , prettyFieldGrammar
  ) where

import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Distribution.Fields.Field (FieldName)
import Distribution.Fields.Pretty (PrettyField, PrettyFieldWith (..))
import Distribution.Parsec.Position
import Distribution.Pretty (Pretty (..), showFreeText, showFreeTextV3)
import Distribution.Trivia
import Distribution.Types.Modify
import Distribution.Utils.Generic (toUTF8BS)
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Prelude ()

import Data.Kind

import Distribution.FieldGrammar.Class
import qualified Distribution.Types.Modify as Mod

-- TODO(leana8959): maybe we can compare this to [Field Position] and thus form a roundtrip test.
newtype PrettyFieldGrammar (m :: Mod.HasAnnotation) s a = PrettyFG
  { fieldGrammarPretty :: CabalSpecVersion -> s -> PrettyFieldGrammarOut m
  }
  deriving (Functor)

-- Toggle between legacy print and new exact print
--
-- Sections are not modeled in Field grammar.
-- PrettyFieldGrammar should output a field and not think about sections.
--
-- Outputting sections in field grammar would require us to fuse elements of the same section together.
type family PrettyFieldGrammarOut (m :: Mod.HasAnnotation) where
  PrettyFieldGrammarOut Mod.HasNoAnn = [PrettyField]
  PrettyFieldGrammarOut Mod.HasAnn =
      [ ( Maybe Position
        , (Position, FieldName)
        , (Position, PP.Doc)
        )
      ]

instance Applicative (PrettyFieldGrammar Mod.HasNoAnn s) where
  pure _ = PrettyFG (\_ _ -> mempty)
  PrettyFG f <*> PrettyFG x = PrettyFG (\v s -> f v s <> x v s)

instance Applicative (PrettyFieldGrammar Mod.HasAnn s) where
  pure _ = PrettyFG (\_ _ -> mempty)
  PrettyFG f <*> PrettyFG x = PrettyFG (\v s -> f v s <> x v s)

-- | We can use 'PrettyFieldGrammar' to pp print the @s@.
--
-- /Note:/ there is not trailing @($+$ text "")@.
prettyFieldGrammar :: CabalSpecVersion -> PrettyFieldGrammar m s a -> s -> PrettyFieldGrammarOut m
prettyFieldGrammar = flip fieldGrammarPretty

instance FieldGrammarWith Mod.HasNoAnn Pretty PrettyFieldGrammar where
  blurFieldGrammar f (PrettyFG pp) = PrettyFG (\v -> pp v . aview f)

  uniqueFieldAla fn _pack l = PrettyFG $ \_v s ->
    ppField fn (pretty (pack' _pack (aview l s)))

  uniqueFieldAla' = uniqueFieldAla

  booleanFieldDef fn l def = PrettyFG pp
    where
      pp _v s
        | b == def = mempty
        | otherwise = ppField fn (PP.text (show b))
        where
          b = aview l s

  booleanFieldDef' = booleanFieldDef

  optionalFieldAla fn _pack l = PrettyFG pp
    where
      pp v s = case aview l s of
        Nothing -> mempty
        Just a -> ppField fn (prettyVersioned v (pack' _pack a))

  optionalFieldDefAla fn _pack l def = PrettyFG pp
    where
      pp v s
        | x == def = mempty
        | otherwise = ppField fn (prettyVersioned v (pack' _pack x))
        where
          x = aview l s

  freeTextField fn l = PrettyFG pp
    where
      pp v s = maybe mempty (ppField fn . showFT) (aview l s)
        where
          showFT
            | v >= CabalSpecV3_0 = showFreeTextV3
            | otherwise = showFreeText

  -- it's ok to just show, as showFreeText of empty string is empty.
  freeTextFieldDef fn l = PrettyFG pp
    where
      pp v s = ppField fn (showFT (aview l s))
        where
          showFT
            | v >= CabalSpecV3_0 = showFreeTextV3
            | otherwise = showFreeText

  freeTextFieldDefST = defaultFreeTextFieldDefST

  monoidalFieldAla fn _pack l = PrettyFG pp
    where
      pp v s = ppField fn (prettyVersioned v (pack' _pack (aview l s)))

  monoidalFieldAla' = monoidalFieldAla

  prefixedFields _fnPfx l = PrettyFG (\_ -> pp . aview l)
    where
      pp xs =
        -- always print the field, even its Doc is empty.
        -- i.e. don't use ppField
        [ PrettyField (toUTF8BS n) $ PP.vcat $ map PP.text $ lines s
        | (n, s) <- xs
        -- fnPfx `isPrefixOf` n
        ]

  knownField _ = pure ()
  deprecatedSince _ _ x = x

  -- TODO: as PrettyFieldGrammar isn't aware of cabal-version: we output the field
  -- this doesn't affect roundtrip as `removedIn` fields cannot be parsed
  -- so invalid documents can be only manually constructed.
  removedIn _ _ x = x
  availableSince _ _ = id
  hiddenField _ = PrettyFG (\_ -> mempty)

instance FieldGrammarWith Mod.HasAnn Pretty PrettyFieldGrammar where
  blurFieldGrammar f (PrettyFG pp) = PrettyFG (\v -> pp v . aview f)

  uniqueFieldAla fn _pack l = PrettyFG $ \_v s ->
    ppFieldFakePos fn (pretty (pack' _pack (aview l s)))

  booleanFieldDef fn l def = PrettyFG pp
    where
      pp _v s
        | b == def = mempty
        | otherwise = ppFieldFakePos fn (PP.text (show b))
        where
          b = aview l s

  optionalFieldAla fn _pack l = PrettyFG pp
    where
      pp v s = case aview l s of
        Nothing -> mempty
        Just a -> ppFieldFakePos fn (prettyVersioned v (pack' _pack a))

  optionalFieldDefAla fn _pack l def = PrettyFG pp
    where
      pp v s
        | x == def = mempty
        | otherwise = ppFieldFakePos fn (prettyVersioned v (pack' _pack x))
        where
          x = aview l s

  freeTextField fn l = PrettyFG pp
    where
      pp v s = maybe mempty (ppFieldFakePos fn . showFT) (aview l s)
        where
          showFT
            | v >= CabalSpecV3_0 = showFreeTextV3
            | otherwise = showFreeText

  -- it's ok to just show, as showFreeText of empty string is empty.
  freeTextFieldDef fn l = PrettyFG pp
    where
      pp v s = ppFieldFakePos fn (showFT (aview l s))
        where
          showFT
            | v >= CabalSpecV3_0 = showFreeTextV3
            | otherwise = showFreeText

  freeTextFieldDefST = defaultFreeTextFieldDefST

  monoidalFieldAla fn _pack l = PrettyFG pp
    where
      pp v s = ppFieldFakePos fn (prettyVersioned v (pack' _pack (aview l s)))

  prefixedFields _fnPfx l = PrettyFG (\_ -> pp . aview l)
    where
      pp xs =
        -- always print the field, even its Doc is empty.
        -- i.e. don't use ppField
        [ (Just zeroPos,,) (zeroPos, toUTF8BS n) (zeroPos, PP.vcat $ map PP.text $ lines s)
        | (n, s) <- xs
        -- fnPfx `isPrefixOf` n
        ]

  knownField _ = pure ()
  deprecatedSince _ _ x = x

  -- TODO: as PrettyFieldGrammar isn't aware of cabal-version: we output the field
  -- this doesn't affect roundtrip as `removedIn` fields cannot be parsed
  -- so invalid documents can be only manually constructed.
  removedIn _ _ x = x
  availableSince _ _ = id
  hiddenField _ = PrettyFG (\_ -> mempty)

  -- New methods

  monoidalFieldAla' fn _pack l = PrettyFG $ \v s ->
    let bs = fmap (prettyVersioned v . pack' _pack) <$> aview l s
     in ppFieldPos fn bs

  booleanFieldDef' fn l def = PrettyFG $ \_v s ->
      aview l s >>= \(Ann t b) -> case t of
          HasTrivia pos -> ppFieldPos fn [(pos, PP.text (show b))]
          IsInserted -> mempty

  optionalFieldDefAla' fn _pack l def = PrettyFG pp
    where
        -- We absorb fields that have no position for the prototype
      pp v s =
        let Ann t u :: Ann Positions Doc = prettyVersioned v . pack' _pack <$> x
        in  case t of
          HasTrivia pos -> ppFieldPos fn [(pos, u)]
          IsInserted -> mempty
        where
          x = aview l s

  uniqueFieldAla'
    :: forall (b :: Type) (s :: Type) (a :: Type)
     . ( Newtype a b
       , Pretty b
       )
    => FieldName
    -- ^ field name
    -> (a -> b)
    -- ^ 'Newtype' pack
    -> ALens' s (Ann Positions a)
    -- ^ lens into the field
    -> PrettyFieldGrammar Mod.HasAnn s (Ann Positions a)
  uniqueFieldAla' fn _pack l = PrettyFG pp
    where
      pp v s =
        let Ann t u :: Ann Positions Doc = prettyVersioned v . pack' _pack <$> x
        in case t of
        -- We absorb fields that have no position for the prototype
          HasTrivia pos -> ppFieldPos fn [(pos, u)]
          IsInserted -> mempty
        where x = aview l s

ppField :: FieldName -> Doc -> [PrettyField]
ppField name fielddoc
  | PP.isEmpty fielddoc = []
  | otherwise = [PrettyField name fielddoc]

ppFieldPos :: FieldName -> [(Positions, Doc)] -> PrettyFieldGrammarOut Mod.HasAnn
ppFieldPos name possFieldDocs =
  [ (,,)
      (sectionPos poss)
      (fieldNamePos poss, name)
      (fieldLinePos poss, fieldDoc)
  | (poss, fieldDoc) <- possFieldDocs
  ]

-- TODO(leana8959): push out position
-- | Doesn't push out real position, tbd
ppFieldFakePos :: FieldName -> Doc -> PrettyFieldGrammarOut Mod.HasAnn
ppFieldFakePos name fieldDoc =
  [ (,,)
      (Just zeroPos)
      (zeroPos, name)
      (zeroPos, fieldDoc)
  ]
