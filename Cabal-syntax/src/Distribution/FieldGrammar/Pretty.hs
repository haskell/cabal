{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-local-binds #-}

module Distribution.FieldGrammar.Pretty
  ( PrettyFieldGrammar (..)
  , prettyFieldGrammar
  , prettierFieldGrammar
  , prettyAnnotatedFieldGrammar
  ) where

import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Distribution.PrettierField
import Distribution.Fields.Field (FieldName)
import Distribution.Fields.Pretty (PrettyField (..))
import Distribution.Pretty (ExactPretty (..), Pretty, showFreeText, showFreeTextV3)
import Distribution.Utils.Generic (toUTF8BS)
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Prelude ()

import Distribution.Types.Annotation
import Distribution.Parsec.Position

import Debug.Pretty.Simple
import Distribution.FieldGrammar.Class

import qualified Data.Map as M

newtype PrettyFieldGrammar s a = PrettyFG
  { fieldGrammarPretty :: CabalSpecVersion -> TriviaTree -> s -> [PrettyField (Maybe Position)]
  }
  deriving (Functor)

instance Applicative (PrettyFieldGrammar s) where
  pure _ = PrettyFG (\_ _ _ -> mempty)
  PrettyFG f <*> PrettyFG x = PrettyFG (\v t s -> f v t s <> x v t s)

noPosition :: PrettyField ann -> PrettyField (Maybe Position)
noPosition = fmap (const Nothing)

-- | We can use 'PrettyFieldGrammar' to pp print the @s@.
--
-- /Note:/ there is not trailing @($+$ text "")@.
prettyFieldGrammar :: CabalSpecVersion -> PrettyFieldGrammar s a -> s -> [PrettyField (Maybe Position)]
prettyFieldGrammar v g = prettyAnnotatedFieldGrammar v mempty g

prettierFieldGrammar :: CabalSpecVersion -> TriviaTree -> PrettyFieldGrammar s a -> s -> [PrettyField (Maybe Position)]
prettierFieldGrammar v t g = prettyAnnotatedFieldGrammar v t g

prettyAnnotatedFieldGrammar
  :: CabalSpecVersion
  -> TriviaTree
  -> PrettyFieldGrammar s a
  -> s
  -> [PrettyField (Maybe Position)]
prettyAnnotatedFieldGrammar v t g = fieldGrammarPretty g v t

instance FieldGrammar ExactPretty PrettyFieldGrammar where
  withScope :: Markable ns => ns -> PrettyFieldGrammar s a -> PrettyFieldGrammar s a
  withScope x (PrettyFG printer) =
    PrettyFG $ \v t s ->
      let t' = unmarkTriviaTree x t
      in  printer v t' s

  blurFieldGrammar f (PrettyFG pp) = PrettyFG (\v t -> pp v t . aview f)

  -- TODO(leana8959): use the trivia in the methods implemented here
  uniqueFieldAla fn _pack l = PrettyFG $ \_v t s ->
    ppField fn (exactPretty mempty (pack' _pack (aview l s)))

  booleanFieldDef fn l def = PrettyFG pp
    where
      pp _v t s
        | b == def = mempty
        | otherwise = ppField fn (PP.text (show b))
        where
          b = aview l s

  optionalFieldAla fn _pack l = PrettyFG pp
    where
      pp v t s = case aview l s of
        Nothing -> mempty
        Just a -> ppField fn (exactPrettyVersioned v mempty (pack' _pack a))

  optionalFieldDefAla fn _pack l def = PrettyFG pp
    where
      pp v t s
        | x == def = mempty
        | otherwise = ppField fn (exactPrettyVersioned v mempty (pack' _pack x))
        where
          x = aview l s

  freeTextField fn l = PrettyFG pp
    where
      pp v t s = maybe mempty (ppField fn . showFT) (aview l s)
        where
          showFT
            | v >= CabalSpecV3_0 = showFreeTextV3
            | otherwise = showFreeText

  -- it's ok to just show, as showFreeText of empty string is empty.
  freeTextFieldDef fn l = PrettyFG pp
    where
      pp v t s = ppField fn (showFT (aview l s))
        where
          showFT
            | v >= CabalSpecV3_0 = showFreeTextV3
            | otherwise = showFreeText

  freeTextFieldDefST = defaultFreeTextFieldDefST

  monoidalFieldAla fn _pack l = PrettyFG pp
    where
      pp v t s =
        let t' = unmarkTriviaTree fn t
         in -- pTrace ("monoidalFieldAla\n" <> show t') $
            -- ppField fn (exactPrettyVersioned v t' (pack' _pack (aview l s)))
            prettierField fn t' (pack' _pack (aview l s))

  prefixedFields _fnPfx l = PrettyFG (\_ t -> map noPosition . pp . aview l)
    where
      pp xs =
        -- always print the field, even its Doc is empty.
        -- i.e. don't use ppField
        [ PrettyField () (toUTF8BS n) $ PP.vcat $ map PP.text $ lines s
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

ppField :: FieldName -> Doc -> [PrettyField (Maybe Position)]
ppField name fielddoc = ppTriviaField name mempty fielddoc

ppTriviaField :: FieldName -> TriviaTree -> Doc  -> [PrettyField (Maybe Position)]
ppTriviaField name tree fielddoc
  | PP.isEmpty fielddoc = []
  | otherwise =
      let mPos = atPosition (justAnnotation tree)
      in  [PrettyField mPos name fielddoc]
