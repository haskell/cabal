{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-local-binds #-}

module Distribution.FieldGrammar.Pretty
  ( PrettyFieldGrammar (..)
  , prettyFieldGrammar
  , exactPrettyFieldGrammar
  , prettyAnnotatedFieldGrammar
  ) where

import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Distribution.Fields.Field (FieldName)
import Distribution.Fields.Pretty (PrettyField (..), PrettyFieldLine (..))
import Distribution.Pretty (ExactPretty (..), Pretty, showFreeText, showFreeTextV3)
import Distribution.Utils.Generic (toUTF8BS)
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Prelude ()

import Distribution.Pretty

import Distribution.Parsec.Position
import Distribution.Types.Annotation

import Debug.Pretty.Simple
import Distribution.FieldGrammar.Class

import qualified Data.Map as M

newtype PrettyFieldGrammar s a = PrettyFG
  { fieldGrammarPretty :: CabalSpecVersion -> TriviaTree -> s -> [PrettyField Trivia]
  }
  deriving (Functor)

instance Applicative (PrettyFieldGrammar s) where
  pure _ = PrettyFG (\_ _ _ -> mempty)
  PrettyFG f <*> PrettyFG x = PrettyFG (\v t s -> f v t s <> x v t s)

noTrivia :: PrettyField ann -> PrettyField Trivia
noTrivia = fmap (const mempty)

-- | We can use 'PrettyFieldGrammar' to pp print the @s@.
--
-- /Note:/ there is not trailing @($+$ text "")@.
prettyFieldGrammar :: CabalSpecVersion -> PrettyFieldGrammar s a -> s -> [PrettyField Trivia]
prettyFieldGrammar v g = prettyAnnotatedFieldGrammar v mempty g

exactPrettyFieldGrammar :: CabalSpecVersion -> TriviaTree -> PrettyFieldGrammar s a -> s -> [PrettyField Trivia]
exactPrettyFieldGrammar v t g = prettyAnnotatedFieldGrammar v t g

prettyAnnotatedFieldGrammar
  :: CabalSpecVersion
  -> TriviaTree
  -> PrettyFieldGrammar s a
  -> s
  -> [PrettyField Trivia]
prettyAnnotatedFieldGrammar v t g = fieldGrammarPretty g v t

instance FieldGrammar ExactPretty PrettyFieldGrammar where
  withScope :: Markable ns => ns -> PrettyFieldGrammar s a -> PrettyFieldGrammar s a
  withScope x (PrettyFG printer) =
    PrettyFG $ \v t s ->
      let t' = unmarkTriviaTree x t
       in printer v t' s

  blurFieldGrammar f (PrettyFG pp) = PrettyFG (\v t -> pp v t . aview f)

  -- TODO(leana8959): use the trivia in the methods implemented here
  uniqueFieldAla fn _pack l = PrettyFG $ \_v t s ->
      ppTriviaField fn (exactPretty mempty (pack' _pack (aview l s)))

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
        Just a -> ppTriviaField fn (exactPrettyVersioned v mempty (pack' _pack a))

  optionalFieldDefAla fn _pack l def = PrettyFG pp
    where
      pp v t s
        | x == def = mempty
        | otherwise = ppTriviaField fn (exactPrettyVersioned v mempty (pack' _pack x))
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
              (\x -> pTrace ("pretty trivia field \n" <> show x) x)
              $ ppTriviaField fn
              $ (\x -> pTrace ("pretty monoidal \n" <> show x) x)
              $ exactPrettyVersioned v t' (pack' _pack (aview l s))

  prefixedFields _fnPfx l = PrettyFG (\_ t -> map noTrivia . pp . aview l)
    where
      pp xs =
        -- always print the field, even its Doc is empty.
        -- i.e. don't use ppField
        [ let docs = map PP.text $ lines s
              pfls = map (PrettyFieldLine mempty) docs
          in  PrettyField () (toUTF8BS n) $ pfls
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

ppField :: FieldName -> Doc -> [PrettyField Trivia]
ppField name fielddoc = ppTriviaField name [DocAnn fielddoc mempty]

ppTriviaField :: FieldName -> [DocAnn TriviaTree] -> [PrettyField Trivia]
ppTriviaField name docAnns
  | null docAnns = []
  | otherwise =
      let proj (DocAnn doc (justAnnotation->ann)) = (PrettyFieldLine ann doc, ann)

          (pfls, anns) = unzip $ map proj docAnns

      in  [PrettyField (mconcat anns) name pfls]
