{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides a 'FieldGrammarParser', one way to parse
-- @.cabal@ -like files.
--
-- Fields can be specified multiple times in the .cabal files.  The order of
-- such entries is important, but the mutual ordering of different fields is
-- not.Also conditional sections are considered after non-conditional data.
-- The example of this silent-commutation quirk is the fact that
--
-- @
-- buildable: True
-- if os(linux)
--   buildable: False
-- @
--
-- and
--
-- @
-- if os(linux)
--   buildable: False
-- buildable: True
-- @
--
-- behave the same! This is the limitation of 'GeneralPackageDescription'
-- structure.
--
-- So we transform the list of fields @['Field' ann]@ into
-- a map of grouped ordinary fields and a list of lists of sections:
-- @'Fields' ann = 'Map' 'FieldName' ['NamelessField' ann]@ and @[['Section' ann]]@.
--
-- We need list of list of sections, because we need to distinguish situations
-- where there are fields in between. For example
--
-- @
-- if flag(bytestring-lt-0_10_4)
--   build-depends: bytestring < 0.10.4
--
-- default-language: Haskell2020
--
-- else
--   build-depends: bytestring >= 0.10.4
--
-- @
--
-- is obviously invalid specification.
--
-- We can parse 'Fields' like we parse @aeson@ objects, yet we use
-- slightly higher-level API, so we can process unspecified fields,
-- to report unknown fields and save custom @x-fields@.
module Distribution.FieldGrammar.Parsec
  ( ParsecFieldGrammar
  , ParsecFieldGrammarWith
  , parseFieldGrammar
  , parseFieldGrammarCheckingStanzas
  , fieldGrammarKnownFieldList

    -- * Auxiliary
  , Fields
  , FieldAnn (..)
  , FieldLineAnn (..)

  , NamelessField (..)
  , namelessFieldAnn
  , Section (..)
  , runFieldParser
  , runFieldParser'
  , fieldLinesToStream
  , freeTextIgnoreDotlineVers
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.String (trim)
import Prelude ()

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Utils.ShortText as ShortText
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P

import Distribution.CabalSpecVersion
import Distribution.FieldGrammar.Class
import Distribution.Fields.Field
import Distribution.Fields.ParseResult
import Distribution.Parsec
import Distribution.Parsec.FieldLineStream
import Distribution.Parsec.Position (positionCol, positionRow)
import Distribution.Types.Trivia

import Data.Kind

import Distribution.Types.Annotation

-------------------------------------------------------------------------------
-- Auxiliary types
-------------------------------------------------------------------------------

type Fields s t = Map FieldName [NamelessField s t]

data FieldAnn = FieldAnn { faPosition :: !Position, faComments :: ![Comment Position], faCasedName :: !BS.ByteString }
data FieldLineAnn = FieldLineAnn { flaPosition :: !Position, flaComments :: ![Comment Position] }

-- | Single field, without name, but with its annotation.
data NamelessField s t = MkNamelessField !s [FieldLine t]
  deriving (Eq, Show, Functor)

namelessFieldAnn :: NamelessField s t -> s
namelessFieldAnn (MkNamelessField s _) = s

-- | The 'Section' constructor of 'Field'.
data Section ann = MkSection !(Name ann) [SectionArg ann] [Field ann]
  deriving (Eq, Show, Functor)

-------------------------------------------------------------------------------
-- ParsecFieldGrammar
-------------------------------------------------------------------------------

type ParsecFieldGrammar = ParsecFieldGrammarWith Abst

data ParsecFieldGrammarWith (m :: ParsingPhase) s a = ParsecFG
  { fieldGrammarKnownFields :: !(Set FieldName)
  , fieldGrammarKnownPrefixes :: !(Set FieldName)
  , fieldGrammarParser :: forall src. (CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParseResult src a)
  }
  deriving (Functor)

unCommentFields :: Fields FieldAnn FieldLineAnn -> Fields FieldAnn Position
unCommentFields = (fmap . fmap . fmap) flaPosition

parseFieldGrammar :: CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParsecFieldGrammarWith m s a -> ParseResult src a
parseFieldGrammar v fields grammar = do
  for_ (Map.toList (Map.filterWithKey (isUnknownField grammar) fields)) $ \(name, nfields) ->
    for_ nfields $ \(MkNamelessField fa _) ->
      parseWarning (faPosition fa) PWTUnknownField $ "Unknown field: " ++ show name
  -- TODO: fields allowed in this section

  -- parse
  fieldGrammarParser grammar v fields

isUnknownField :: ParsecFieldGrammarWith m s a -> FieldName -> [NamelessField FieldAnn FieldLineAnn] -> Bool
isUnknownField grammar k _ =
  not $
    k `Set.member` fieldGrammarKnownFields grammar
      || any (`BS.isPrefixOf` k) (fieldGrammarKnownPrefixes grammar)

-- | Parse a ParsecFieldGrammar and check for fields that should be stanzas.
parseFieldGrammarCheckingStanzas :: CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParsecFieldGrammarWith m s a -> Set BS.ByteString -> ParseResult src a
parseFieldGrammarCheckingStanzas v fields grammar sections = do
  for_ (Map.toList (Map.filterWithKey (isUnknownField grammar) fields)) $ \(name, nfields) ->
    for_ nfields $ \(MkNamelessField fa _) ->
      if name `Set.member` sections
        then parseFailure (faPosition fa) $ "'" ++ fromUTF8BS name ++ "' is a stanza, not a field. Remove the trailing ':' to parse a stanza."
        else parseWarning (faPosition fa) PWTUnknownField $ "Unknown field: " ++ show name

  fieldGrammarParser grammar v fields

fieldGrammarKnownFieldList :: ParsecFieldGrammarWith m s a -> [FieldName]
fieldGrammarKnownFieldList = Set.toList . fieldGrammarKnownFields

-- TODO(leana8959): This means that a comment previously associated with the fieldname will now be associated with a fieldline.
-- Doesn't sound too good to me.
-- Maybe add a field to FieldLineAnn type.
extractCommentsField :: NamelessField FieldAnn FieldLineAnn -> ([Comment Position], BS.ByteString, NamelessField Position Position)
extractCommentsField (MkNamelessField fa fls) =
  let FieldAnn {faPosition, faComments, faCasedName} = fa
      (fieldComments, fields') = unzip $ map extractCommentsFieldLine fls
  in  (faComments ++ concat fieldComments, faCasedName, MkNamelessField faPosition fields')

extractCommentsFieldLine :: FieldLine FieldLineAnn -> ([Comment Position], FieldLine Position)
extractCommentsFieldLine (FieldLine fla bs) =
  let FieldLineAnn {flaPosition, flaComments} = fla
  in  (flaComments, FieldLine flaPosition bs)

instance Applicative (ParsecFieldGrammarWith m s) where
  pure x = ParsecFG mempty mempty (\_ _ -> pure x)
  {-# INLINE pure #-}

  ParsecFG f f' f'' <*> ParsecFG x x' x'' =
    ParsecFG
      (mappend f x)
      (mappend f' x')
      (\v fields -> f'' v fields <*> x'' v fields)
  {-# INLINE (<*>) #-}

warnMultipleSingularFields :: FieldName -> [NamelessField FieldAnn FieldLineAnn] -> ParseResult src ()
warnMultipleSingularFields _ [] = pure ()
warnMultipleSingularFields fn (x : xs) = do
  let pos = faPosition $ namelessFieldAnn x
      poss = map (faPosition . namelessFieldAnn) xs
  parseWarning pos PWTMultipleSingularField $
    "The field " <> show fn <> " is specified more than once at positions " ++ intercalate ", " (map showPos (pos : poss))

instance FieldGrammarWith Abst Parsec ParsecFieldGrammarWith where
  blurFieldGrammar _ (ParsecFG s s' parser) = ParsecFG s s' parser

  uniqueFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> parseFatalFailure zeroPos $ show fn ++ " field missing"
        Just [] -> parseFatalFailure zeroPos $ show fn ++ " field missing"
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) =
        unpack' _pack <$> runFieldParser pos parsec v fls

  uniqueFieldAla' = uniqueFieldAla

  booleanFieldDef fn _extract def = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure def
        Just [] -> pure def
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = runFieldParser pos parsec v fls

  booleanFieldDef' = booleanFieldDef

  optionalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure Nothing
        Just [] -> pure Nothing
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls))
        | null fls = pure Nothing
        | otherwise = Just . unpack' _pack <$> runFieldParser pos parsec v fls

  optionalFieldDefAla' = optionalFieldDefAla

  optionalFieldDefAla fn _pack _extract def = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure def
        Just [] -> pure def
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls))
        | null fls = pure def
        | otherwise = unpack' _pack <$> runFieldParser pos parsec v fls

  freeTextField fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure Nothing
        Just [] -> pure Nothing
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (extractCommentsField->(cmts, _caseName, MkNamelessField pos fls))
        | null fls = pure Nothing
        | v >= freeTextIgnoreDotlineVers = pure (Just (fieldlinesToFreeText3 pos fls))
        | otherwise = pure (Just (fieldlinesToFreeText fls))

  freeTextFieldDef fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure ""
        Just [] -> pure ""
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls))
        | null fls = pure ""
        | v >= freeTextIgnoreDotlineVers = pure (fieldlinesToFreeText3 pos fls)
        | otherwise = pure (fieldlinesToFreeText fls)

  -- freeTextFieldDefST = defaultFreeTextFieldDefST
  freeTextFieldDefST fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure mempty
        Just [] -> pure mempty
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = case fls of
        [] -> pure mempty
        [FieldLine _ bs] -> pure (ShortText.unsafeFromUTF8BS bs)
        _
          | v >= freeTextIgnoreDotlineVers -> pure (ShortText.toShortText $ fieldlinesToFreeText3 pos fls)
          | otherwise -> pure (ShortText.toShortText $ fieldlinesToFreeText fls)

  monoidalFieldAla
    :: forall m b a s
     . (Parsec b, Monoid a, Newtype a b)
    => FieldName
    -> (a -> b)
    -> ALens' s a
    -> ParsecFieldGrammarWith m s a
  monoidalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser :: CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParseResult src a
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure mempty
        Just xs -> foldMap (unpack' _pack) <$> traverse (parseOne v) xs

      parseOne :: CabalSpecVersion -> NamelessField FieldAnn FieldLineAnn -> ParseResult src b
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = runFieldParser pos parsec v fls

  monoidalFieldAla' = monoidalFieldAla

  prefixedFields fnPfx _extract = ParsecFG mempty (Set.singleton fnPfx) (\_v -> pure . parser)
    where
      parser :: Fields FieldAnn FieldLineAnn -> [(String, String)]
      parser (unCommentFields->values) = reorder $ concatMap convert $ filter match $ Map.toList values

      match (fn, _) = fnPfx `BS.isPrefixOf` fn
      convert (fn, fields) =
        [ (pos, (fromUTF8BS fn, trim $ fromUTF8BS $ fieldlinesToBS fls))
        | MkNamelessField pos fls <- fields
        ]
      -- hack: recover the order of prefixed fields
      reorder = map snd . sortBy (comparing $ faPosition . fst)

  availableSince vs def (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v commentedValues
        | v >= vs = parser v commentedValues
        | otherwise = do
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList unknownFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField fa _) ->
                parseWarning (faPosition fa) PWTUnknownField $
                  "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ ". This field will be ignored."

            pure def
        where
          values = unCommentFields commentedValues

  availableSinceWarn vs (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v commentedValues
        | v >= vs = parser v commentedValues
        | otherwise = do
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList unknownFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField fa _) ->
                parseWarning (faPosition fa) PWTUnknownField $
                  "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ "."

            parser v commentedValues
        where
          values = unCommentFields commentedValues

  -- todo we know about this field
  deprecatedSince vs msg (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v commentedValues
        | v >= vs = do
            let deprecatedFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList deprecatedFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField fa _) ->
                parseWarning (faPosition fa) PWTDeprecatedField $
                  "The field " <> show name <> " is deprecated in the Cabal specification version " ++ showCabalSpecVersion vs ++ ". " ++ msg

            parser v commentedValues
        | otherwise = parser v commentedValues
        where
          values = unCommentFields commentedValues

  removedIn vs msg (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v commentedValues
        | v >= vs = do
            let msg' = if null msg then "" else ' ' : msg
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            let nameFa =
                  [ (name, fa)
                  | (name, fields) <- Map.toList unknownFields
                  , MkNamelessField fa _ <- fields
                  ]

            let makeMsg name = "The field " <> show name <> " is removed in the Cabal specification version " ++ showCabalSpecVersion vs ++ "." ++ msg'

            case nameFa of
              -- no fields => proceed (with empty values, to be sure)
              [] -> parser v mempty
              -- if there's single field: fail fatally with it
              ((name, fa) : rest) -> do
                for_ rest $ \(name', fa') -> parseFailure (faPosition fa') $ makeMsg name'
                parseFatalFailure (faPosition fa) $ makeMsg name
        | otherwise = parser v commentedValues
        where
          values = unCommentFields commentedValues

  knownField fn = ParsecFG (Set.singleton fn) Set.empty (\_ _ -> pure ())

  hiddenField = id

instance FieldGrammarWith Conc Parsec ParsecFieldGrammarWith where
  -- Old methods
  blurFieldGrammar _ (ParsecFG s s' parser) = ParsecFG s s' parser
  uniqueFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> parseFatalFailure zeroPos $ show fn ++ " field missing"
        Just [] -> parseFatalFailure zeroPos $ show fn ++ " field missing"
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) =
        unpack' _pack <$> runFieldParser pos parsec v fls
  booleanFieldDef fn _extract def = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure def
        Just [] -> pure def
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = runFieldParser pos parsec v fls
  optionalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure Nothing
        Just [] -> pure Nothing
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls))
        | null fls = pure Nothing
        | otherwise = Just . unpack' _pack <$> runFieldParser pos parsec v fls
  optionalFieldDefAla fn _pack _extract def = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure def
        Just [] -> pure def
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls))
        | null fls = pure def
        | otherwise = unpack' _pack <$> runFieldParser pos parsec v fls
  freeTextField fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure Nothing
        Just [] -> pure Nothing
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls))
        | null fls = pure Nothing
        | v >= freeTextIgnoreDotlineVers = pure (Just (fieldlinesToFreeText3 pos fls))
        | otherwise = pure (Just (fieldlinesToFreeText fls))
  freeTextFieldDef fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure ""
        Just [] -> pure ""
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls))
        | null fls = pure ""
        | v >= freeTextIgnoreDotlineVers = pure (fieldlinesToFreeText3 pos fls)
        | otherwise = pure (fieldlinesToFreeText fls)
  freeTextFieldDefST fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure mempty
        Just [] -> pure mempty
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = case fls of
        [] -> pure mempty
        [FieldLine _ bs] -> pure (ShortText.unsafeFromUTF8BS bs)
        _
          | v >= freeTextIgnoreDotlineVers -> pure (ShortText.toShortText $ fieldlinesToFreeText3 pos fls)
          | otherwise -> pure (ShortText.toShortText $ fieldlinesToFreeText fls)
  monoidalFieldAla
    :: forall m b a s
     . (Parsec b, Monoid a, Newtype a b)
    => FieldName
    -> (a -> b)
    -> ALens' s a
    -> ParsecFieldGrammarWith m s a
  monoidalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser :: CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParseResult src a
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure mempty
        Just xs -> foldMap (unpack' _pack) <$> traverse (parseOne v) xs
      parseOne :: CabalSpecVersion -> NamelessField FieldAnn FieldLineAnn -> ParseResult src b
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = runFieldParser pos parsec v fls

  prefixedFields fnPfx _extract = ParsecFG mempty (Set.singleton fnPfx) (\_ fs -> pure (parser fs))
    where
      parser :: Fields FieldAnn FieldLineAnn -> [(String, String)]
      parser values = reorder $ concatMap convert $ filter match $ Map.toList values
      match (fn, _) = fnPfx `BS.isPrefixOf` fn
      convert (fn, fields) =
        [ (pos, (fromUTF8BS fn, trim $ fromUTF8BS $ fieldlinesToBS fls))
        | MkNamelessField pos fls <- fields
        ]
      -- hack: recover the order of prefixed fields
      reorder = map snd . sortBy (comparing $ faPosition . fst)
  availableSince vs def (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v commentedValues
        | v >= vs = parser v commentedValues
        | otherwise = do
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList unknownFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField fa _) ->
                parseWarning (faPosition fa) PWTUnknownField $
                  "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ ". This field will be ignored."
            pure def
          where
            values = unCommentFields commentedValues

  availableSinceWarn vs (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v commentedValues
        | v >= vs = parser v commentedValues
        | otherwise = do
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList unknownFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField fa _) ->
                parseWarning (faPosition fa) PWTUnknownField $
                  "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ "."
            parser v commentedValues
          where
            values = unCommentFields commentedValues

  deprecatedSince vs msg (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v commentedValues
        | v >= vs = do
            let deprecatedFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList deprecatedFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField fa _) ->
                parseWarning (faPosition fa) PWTDeprecatedField $
                  "The field " <> show name <> " is deprecated in the Cabal specification version " ++ showCabalSpecVersion vs ++ ". " ++ msg
            parser v commentedValues
        | otherwise = parser v commentedValues
        where
          values = unCommentFields commentedValues
  removedIn vs msg (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v commentedValues
        | v >= vs = do
            let msg' = if null msg then "" else ' ' : msg
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            let nameFa =
                  [ (name, fa)
                  | (name, fields) <- Map.toList unknownFields
                  , MkNamelessField fa _ <- fields
                  ]
            let makeMsg name = "The field " <> show name <> " is removed in the Cabal specification version " ++ showCabalSpecVersion vs ++ "." ++ msg'
            case nameFa of
              -- no fields => proceed (with empty values, to be sure)
              [] -> parser v mempty
              -- if there's single field: fail fatally with it
              ((name, fa) : rest) -> do
                for_ rest $ \(name', fa') -> parseFailure (faPosition fa') $ makeMsg name'
                parseFatalFailure (faPosition fa) $ makeMsg name
        | otherwise = parser v commentedValues
        where
          values = unCommentFields commentedValues
  knownField fn = ParsecFG (Set.singleton fn) Set.empty (\_ _ -> pure ())
  hiddenField = id

  -- New methods

  booleanFieldDef'
    :: forall s
     . FieldName
    -- \^ field name
    -> ALens' s [Ann Positions Bool]
    -- \^ lens into the field
    -> Bool
    -- \^ default
    -> ParsecFieldGrammarWith Conc s [Ann Positions Bool]
  booleanFieldDef' fn _extract def = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser :: CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParseResult src [Ann Positions Bool]
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure def'
        Just [] -> pure def'
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
        where
          def' = [Ann IsInserted def]

      parseOne :: CabalSpecVersion -> NamelessField FieldAnn FieldLineAnn -> ParseResult src [Ann Positions Bool]
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = do
        (flPos, x) <- runFieldParser pos (liftA2 (,) getPosition parsec) v fls
        pure . (: []) $ Ann (HasTrivia $ Positions Nothing pos flPos) x

  -- TODO(leana8959): implement all methods

  -- This function allows us to manage the position coming from a parsed field
  -- In the printer, it can... IDK? Annotate the pretty doc position?
  --
  -- - merging is defered
  -- - position is retained in each result
  monoidalFieldAla'
    :: forall b a s
     . (Parsec b, Newtype a b)
    => FieldName
    -> (a -> b)
    -> ALens' s [([Comment Position], (Positions, a))]
    -> ParsecFieldGrammarWith Conc s [([Comment Position], (Positions, a))]
  monoidalFieldAla' fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser :: CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParseResult src [([Comment Position], (Positions, a))]
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure mempty
        Just xs -> (map . fmap . fmap) (unpack' _pack) <$> traverse (parseOne v) xs

      parseOne :: CabalSpecVersion -> NamelessField FieldAnn FieldLineAnn -> ParseResult src ([Comment Position], (Positions, b))
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = do
        (flPos, x) <- runFieldParser pos (liftA2 (,) getPosition parsec) v fls
        pure (cmts, (Positions Nothing pos flPos, x))

  optionalFieldDefAla'
    :: forall b a s
     . (Parsec b, Newtype a b)
    => FieldName
    -- \^ field name
    -> (a -> b)
    -- \^ 'Newtype' pack
    -> ALens' s (Ann Positions a)
    -- \^ @'Lens'' s a@: lens into the field
    -> a
    -- \^ default value
    -> ParsecFieldGrammarWith Conc s (Ann Positions a)
  optionalFieldDefAla' fn _pack _extract def = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser :: CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParseResult src (Ann Positions a)
      parser v fields = case Map.lookup fn fields of
        Nothing -> pure def'
        Just [] -> pure def'
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne :: CabalSpecVersion -> NamelessField FieldAnn FieldLineAnn -> ParseResult src (Ann Positions a)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls))
        | null fls = pure def'
        | otherwise = do
            (flPos, x) <- runFieldParser pos (liftA2 (,) getPosition (parsec @b)) v fls
            pure (Ann (HasTrivia $ Positions Nothing pos flPos) (unpack' _pack x))

      def' :: Ann Positions a
      def' = Ann IsInserted def

  uniqueFieldAla'
    :: forall (b :: Type) (s :: Type) (a :: Type)
     . ( Newtype a b
       , Parsec b
       )
    => FieldName
    -- \^ field name
    -> (a -> b)
    -- \^ 'Newtype' pack
    -> ALens' s (Ann Positions a)
    -- \^ lens into the field
    -> ParsecFieldGrammarWith Conc s (Ann Positions a)
  uniqueFieldAla' fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser :: CabalSpecVersion -> Fields FieldAnn FieldLineAnn -> ParseResult src (Ann Positions a)
      parser v fields = case Map.lookup fn fields of
        Nothing -> parseFatalFailure zeroPos $ show fn ++ " field missing"
        Just [] -> parseFatalFailure zeroPos $ show fn ++ " field missing"
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)
        where

      parseOne :: CabalSpecVersion -> NamelessField FieldAnn FieldLineAnn -> ParseResult src (Ann Positions a)
      parseOne v (extractCommentsField->(cmts, _casedName, MkNamelessField pos fls)) = do
        (flPos, x) <- runFieldParser pos (liftA2 (,) getPosition (parsec @b)) v fls
        pure (Ann (HasTrivia $ Positions Nothing pos flPos) (unpack' _pack x))

-------------------------------------------------------------------------------
-- Parsec
-------------------------------------------------------------------------------

runFieldParser' :: [Position] -> ParsecParser a -> CabalSpecVersion -> FieldLineStream -> ParseResult src a
runFieldParser' inputPoss p v str = case P.runParser p' [] "<field>" str of
  Right (pok, ws) -> do
    traverse_ (\(PWarning t pos w) -> parseWarning (mapPosition pos) t w) ws
    pure pok
  Left err -> do
    let ppos = P.errorPos err
    let epos = mapPosition $ Position (P.sourceLine ppos) (P.sourceColumn ppos)

    let msg =
          P.showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
            (P.errorMessages err)
    parseFatalFailure epos $ msg ++ "\n"
  where
    p' = (,) <$ P.spaces <*> unPP p v <* P.spaces <* P.eof <*> P.getState

    -- Positions start from 1:1, not 0:0
    mapPosition (Position prow pcol) = go (prow - 1) inputPoss
      where
        go _ [] = zeroPos
        go _ [Position row col] = Position row (col + pcol - 1)
        go n (Position row col : _) | n <= 0 = Position row (col + pcol - 1)
        go n (_ : ps) = go (n - 1) ps

-- TODO(leana8959): relax the argument to take in comments
runFieldParser :: Position -> ParsecParser a -> CabalSpecVersion -> [FieldLine Position] -> ParseResult src a
runFieldParser pp p v ls = runFieldParser' poss p v $ fieldLinesToStream ls
  where
    poss :: [Position]
    poss = map (\(FieldLine ann _) -> ann) ls ++ [pp] -- add "default" position

fieldlinesToBS :: [FieldLine ann] -> BS.ByteString
fieldlinesToBS = BS.intercalate "\n" . map (\(FieldLine _ bs) -> bs)

-- Example package with dot lines
-- http://hackage.haskell.org/package/copilot-cbmc-0.1/copilot-cbmc.cabal
fieldlinesToFreeText :: [FieldLine ann] -> String
fieldlinesToFreeText [FieldLine _ "."] = "."
fieldlinesToFreeText fls = intercalate "\n" (map go fls)
  where
    go (FieldLine _ bs)
      | s == "." = ""
      | otherwise = s
      where
        s = trim (fromUTF8BS bs)

-- | Cabal version where we switch from the old free text parser that had
-- special logic for "dotlines" to a new parser that has no such logic.
freeTextIgnoreDotlineVers :: CabalSpecVersion
freeTextIgnoreDotlineVers = CabalSpecV3_0

fieldlinesToFreeText3 :: Position -> [FieldLine Position] -> String
fieldlinesToFreeText3 _ [] = ""
fieldlinesToFreeText3 _ [FieldLine _ bs] = fromUTF8BS bs
fieldlinesToFreeText3 pos (FieldLine pos1 bs1 : fls2@(FieldLine pos2 _ : _))
  -- if first line is on the same line with field name:
  -- the indentation level is either
  -- 1. the indentation of left most line in rest fields
  -- 2. the indentation of the first line
  -- whichever is leftmost
  | positionRow pos == positionRow pos1 =
      concat $
        fromUTF8BS bs1
          : mealy (mk mcol1) pos1 fls2
  -- otherwise, also indent the first line
  | otherwise =
      concat $
        replicate (positionCol pos1 - mcol2) ' '
          : fromUTF8BS bs1
          : mealy (mk mcol2) pos1 fls2
  where
    mcol1 = foldl' (\a b -> min a $ positionCol $ fieldLineAnn b) (min (positionCol pos1) (positionCol pos2)) fls2
    mcol2 = foldl' (\a b -> min a $ positionCol $ fieldLineAnn b) (positionCol pos1) fls2

    mk :: Int -> Position -> FieldLine Position -> (Position, String)
    mk col p (FieldLine q bs) =
      ( q
      , replicate newlines '\n'
          ++ replicate indent ' '
          ++ fromUTF8BS bs
      )
      where
        newlines = positionRow q - positionRow p
        indent = positionCol q - col

mealy :: (s -> a -> (s, b)) -> s -> [a] -> [b]
mealy f = go
  where
    go _ [] = []
    go s (x : xs) = let ~(s', y) = f s x in y : go s' xs

fieldLinesToStream :: [FieldLine Position] -> FieldLineStream
fieldLinesToStream = fieldLinesToStream' (Position 1 1)

-- | Fallback to last position when there's no 'FieldLine'
fieldLinesToStream' :: Position -> [FieldLine Position] -> FieldLineStream
fieldLinesToStream' defaultPos [] = FLSLast mempty defaultPos
fieldLinesToStream' defaultPos (FieldLine pos bs : fs) = FLSCons bs pos $ fieldLinesToStream' defaultPos fs
