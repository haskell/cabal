{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Distribution.FieldGrammar.Pretty
  ( PrettyFieldGrammar
  , prettyFieldGrammar
  ) where

import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Distribution.Trivia
import Distribution.Types.Modify
import Distribution.Fields.Field (FieldName)
import Distribution.Fields.Pretty (PrettyField (..))
import Distribution.Pretty (Pretty (..), showFreeText, showFreeTextV3)
import Distribution.Utils.Generic (toUTF8BS)
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Prelude ()

import Distribution.FieldGrammar.Class
import qualified Distribution.Types.Modify as Mod

-- TODO(leana8959): maybe we can compare this to [Field Position] and thus form a roundtrip test.
newtype PrettyFieldGrammar (m :: Mod.HasAnnotation) s a = PrettyFG
  { fieldGrammarPretty :: CabalSpecVersion -> s -> [PrettyField (WithPos m)]
  }
  deriving (Functor)

instance Applicative (PrettyFieldGrammar m s) where
  pure _ = PrettyFG (\_ _ -> mempty)
  PrettyFG f <*> PrettyFG x = PrettyFG (\v s -> f v s <> x v s)

-- | We can use 'PrettyFieldGrammar' to pp print the @s@.
--
-- /Note:/ there is not trailing @($+$ text "")@.
prettyFieldGrammar :: CabalSpecVersion -> PrettyFieldGrammar m s a -> s -> [PrettyField (WithPos m)]
prettyFieldGrammar = flip fieldGrammarPretty

instance FieldGrammarWith Mod.HasNoAnn Pretty PrettyFieldGrammar where
  blurFieldGrammar f (PrettyFG pp) = PrettyFG (\v -> pp v . aview f)

  uniqueFieldAla fn _pack l = PrettyFG $ \_v s ->
    ppField fn (pretty (pack' _pack (aview l s)))

  booleanFieldDef fn l def = PrettyFG pp
    where
      pp _v s
        | b == def = mempty
        | otherwise = ppField fn (PP.text (show b))
        where
          b = aview l s

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

  prefixedFields _fnPfx l = PrettyFG (\_ -> pp . aview l)
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

instance FieldGrammarWith Mod.HasAnn Pretty PrettyFieldGrammar where
  monoidalFieldAla' fn _pack l = PrettyFG $ \v s ->
      let bs :: [(Positions, Doc)] = fmap (prettyVersioned v . pack' _pack) <$> (aview l s)
       in -- ppField fn mempty
          []

  booleanFieldDef' fn l def = PrettyFG $ \_v s ->
      let Ann t b = aview l s
       in -- TODO(leana8959): push out position
           -- ppField fn $ applyTriviaDoc t (PP.text (show b))
           []

ppField :: FieldName -> Doc -> [PrettyField ()]
ppField name fielddoc
  | PP.isEmpty fielddoc = []
  | otherwise = [PrettyField () name fielddoc]

-- NOTE(leana8959): do we need position"s"
ppFieldPos :: FieldName -> Trivia SurroundingText -> [(Positions, Doc)] -> [PrettyField Positions]
ppFieldPos name trivia possFieldDocs = case trivia of
  -- TODO(leana8959): should position always exist (it's a maybe now)

  -- TODO(leana8959): Each fieldDoc should carry their associated fieldname's position
  -- because they don't always have the same position
  -- We then do a post process sorting
  IsInserted -> [] -- Absorb
  _notInserted ->
    possFieldDocs
      >>= \(poss, fieldDoc) -> [ PrettyField undefined name fieldDoc ]
