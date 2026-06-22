{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.FieldGrammar.Pretty
  ( PrettyFieldGrammar
  , PrettyFieldGrammarWith
  , prettyFieldGrammar
  ) where

import Distribution.Fields.Field
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Distribution.Fields.Pretty (PrettyField, PrettyFieldWith (..))
import Distribution.Parsec.Position
import Distribution.Pretty (Pretty (..), showFreeText, showFreeTextV3)
import Distribution.PrettyCtx
import Distribution.Types.Annotation
import Distribution.Types.Trivia
import Distribution.Utils.Generic (toUTF8BS)
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Prelude ()

import Data.Kind

import Distribution.FieldGrammar.Class
import qualified Data.ByteString as BS
import Distribution.Utils.ShortText (fromShortText)

type PrettyFieldGrammar = PrettyFieldGrammarWith Abst

-- TODO(leana8959): maybe we can compare this to [Field Position] and thus form a roundtrip test.
newtype PrettyFieldGrammarWith (m :: ParsingPhase) s a = PrettyFG
  { fieldGrammarPretty :: CabalSpecVersion -> s -> PrettyFieldGrammarOut m
  }
  deriving (Functor)

-- Toggle between legacy print and new exact print
--
-- Sections are not modeled in Field grammar.
-- PrettyFieldGrammar should output a field and not think about sections.
--
-- Outputting sections in field grammar would require us to fuse elements of the same section together.
type family PrettyFieldGrammarOut (m :: ParsingPhase) where
  PrettyFieldGrammarOut Abst = [PrettyField]
  PrettyFieldGrammarOut Conc =
    [ ( (Position, FieldName)
      , (Position, PP.Doc)
      )
    ]

instance Applicative (PrettyFieldGrammarWith Abst s) where
  pure _ = PrettyFG (\_ _ -> mempty)
  PrettyFG f <*> PrettyFG x = PrettyFG (\v s -> f v s <> x v s)

instance Applicative (PrettyFieldGrammarWith Conc s) where
  pure _ = PrettyFG (\_ _ -> mempty)
  PrettyFG f <*> PrettyFG x = PrettyFG (\v s -> f v s <> x v s)

-- | We can use 'PrettyFieldGrammar' to pp print the @s@.
--
-- /Note:/ there is not trailing @($+$ text "")@.
prettyFieldGrammar :: CabalSpecVersion -> PrettyFieldGrammarWith m s a -> s -> PrettyFieldGrammarOut m
prettyFieldGrammar csv fg = fieldGrammarPretty fg csv

instance FieldGrammarWith Abst Pretty PrettyFieldGrammarWith where
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

  optionalFieldDefAla' = optionalFieldDefAla

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

instance FieldGrammarWith Conc PrettyCtx PrettyFieldGrammarWith where
  -- Nothing because subgrammar is not directly within a section?
  blurFieldGrammar f (PrettyFG pp) = PrettyFG (\v -> pp v . aview f)

  -- TODO: push out section position from here
  uniqueFieldAla fn _pack l = PrettyFG $ \_v s ->
    ppFieldFakePos fn (prettyCtx (mempty, pack' _pack (aview l s)))

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
        Just a -> ppFieldFakePos fn (prettyCtxVersioned v (mempty, pack' _pack a))

  optionalFieldDefAla fn _pack l def = PrettyFG pp
    where
      pp v s
        | x == def = mempty
        | otherwise = ppFieldFakePos fn (prettyCtxVersioned v (mempty, pack' _pack x))
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
      pp v s = ppFieldFakePos fn (prettyCtxVersioned v (mempty, pack' _pack (aview l s)))

  prefixedFields _fnPfx l = PrettyFG (\_ -> pp . aview l)
    where
      pp xs =
        -- always print the field, even its Doc is empty.
        -- i.e. don't use ppField
        [ (,) (zeroPos, toUTF8BS n) (zeroPos, PP.vcat $ map PP.text $ lines s)
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
    let bs =
          map
          -- TODO(leana8959): output cased name
            ( \(cmts, casedName, (poss, x)) -> (poss, casedName, prettyCtxVersioned v (cmts, pack' _pack x))
            )
            (aview l s)
     in ppFieldPos' bs

  booleanFieldDef' fn l _def = PrettyFG $ \_v s ->
    aview l s >>= \(Ann t b) -> case t of
      IsInserted -> mempty
      HasTrivia pos -> ppFieldPos fn [(pos, PP.text (show b))]
      _ -> error "unreachable, bad model"

  optionalFieldDefAla' fn _pack l _def = PrettyFG pp
    where
      -- We absorb fields that have no position for the prototype
      pp v s =
        let Ann t u :: Ann Positions Doc = (\u -> prettyCtxVersioned v (mempty, pack' _pack u)) <$> x
         in case t of
              IsInserted -> mempty
              HasTrivia pos -> ppFieldPos fn [(pos, u)]
              -- TODO(leana8959): there shouldn't be any other cases
              _ -> error "unreachable, bad model"
        where
          x = aview l s

  uniqueFieldAla'
    :: forall (b :: Type) (s :: Type) (a :: Type)
     . ( Newtype a b
       , PrettyCtx b
       )
    => FieldName
    -- \^ field name
    -> (a -> b)
    -- \^ 'Newtype' pack
    -> ALens' s (Positions, BS.ByteString, a)
    -- \^ lens into the field
    -> PrettyFieldGrammarWith Conc s (Positions, BS.ByteString, a)
  uniqueFieldAla' _fn _pack l = PrettyFG pp
    where
      pp v s =
        let (poss, casedName, d)  = prettyCtxVersioned v . (mempty,) . pack' _pack <$> x
         in ppFieldPos casedName [(poss, d)]
        where
          x = aview l s
  freeTextFieldDefST' fn l = PrettyFG pp
    where
      pp v s =
        let showFT
              | v >= CabalSpecV3_0 = showFreeTextV3 . fromShortText
              | otherwise = showFreeText . fromShortText

            (poss, casedName, d) = showFT <$> aview l s
        in  ppFieldPos casedName [(poss, d)]

ppField :: FieldName -> Doc -> [PrettyField]
ppField name fielddoc
  | PP.isEmpty fielddoc = []
  | otherwise = [PrettyField name fielddoc]

ppFieldPos :: FieldName -> [(Positions, Doc)] -> PrettyFieldGrammarOut Conc
ppFieldPos name possFieldDocs =
  [ (,) (fieldNamePos poss, name) (fieldLinePos poss, fieldDoc)
  | (poss, fieldDoc) <- possFieldDocs
  ]

ppFieldPos' :: [(Positions, FieldName, Doc)] -> PrettyFieldGrammarOut Conc
ppFieldPos' possFieldDocs =
  [ (,) (fieldNamePos poss, fieldName) (fieldLinePos poss, fieldDoc)
  | (poss, fieldName, fieldDoc) <- possFieldDocs
  ]

-- TODO(leana8959): push out position

-- | Doesn't push out real position, tbd
ppFieldFakePos :: FieldName -> Doc -> PrettyFieldGrammarOut Conc
ppFieldFakePos name fieldDoc =
  [ (,)
      (zeroPos, name)
      (zeroPos, fieldDoc)
  ]
