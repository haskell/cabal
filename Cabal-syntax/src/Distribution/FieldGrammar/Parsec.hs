{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
  ( ParsecFieldGrammar (..)
  , parseFieldGrammar
  , parseFieldGrammarCheckingStanzas
  , fieldGrammarKnownFieldList

    -- * Auxiliary
  , Fields
  , NamelessField (..)
  , namelessFieldAnn
  , Section (..)
  , runFieldParser
  , runFieldParser'
  , fieldLinesToStream
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.String (trim)
import Prelude ()

import Debug.Pretty.Simple

import Distribution.Types.Annotation

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

-------------------------------------------------------------------------------
-- Auxiliary types
-------------------------------------------------------------------------------

type Fields ann = Map FieldName [NamelessField ann]

-- | Single field, without name, but with its annotation.
data NamelessField ann = MkNamelessField !ann [FieldLine ann]
  deriving (Eq, Show, Functor)

namelessFieldAnn :: NamelessField ann -> ann
namelessFieldAnn (MkNamelessField ann _) = ann

-- | The 'Section' constructor of 'Field'.
data Section ann = MkSection !(Name ann) [SectionArg ann] [Field ann]
  deriving (Eq, Show, Functor)

-------------------------------------------------------------------------------
-- ParsecFieldGrammar
-------------------------------------------------------------------------------

data ParsecFieldGrammar s a = ParsecFG
  { fieldGrammarKnownFields :: !(Set FieldName)
  , fieldGrammarKnownPrefixes :: !(Set FieldName)
  , fieldGrammarParser :: forall src. (CabalSpecVersion -> Fields Position -> ParseResult src (TriviaTree, a))
  }
  deriving (Functor)

parseFieldGrammar :: CabalSpecVersion -> Fields Position -> ParsecFieldGrammar s a -> ParseResult src (TriviaTree, a)
parseFieldGrammar v fields grammar = do
  for_ (Map.toList (Map.filterWithKey (isUnknownField grammar) fields)) $ \(name, nfields) ->
    for_ nfields $ \(MkNamelessField pos _) ->
      parseWarning pos PWTUnknownField $ "Unknown field: " ++ show name
  -- TODO: fields allowed in this section

  -- parse
  fieldGrammarParser grammar v fields

isUnknownField :: ParsecFieldGrammar s a -> FieldName -> [NamelessField Position] -> Bool
isUnknownField grammar k _ =
  not $
    k `Set.member` fieldGrammarKnownFields grammar
      || any (`BS.isPrefixOf` k) (fieldGrammarKnownPrefixes grammar)

-- | Parse a ParsecFieldGrammar and check for fields that should be stanzas.
parseFieldGrammarCheckingStanzas
  :: CabalSpecVersion -> Fields Position -> ParsecFieldGrammar s a -> Set BS.ByteString -> ParseResult src (TriviaTree, a)
parseFieldGrammarCheckingStanzas v fields grammar sections = do
  for_ (Map.toList (Map.filterWithKey (isUnknownField grammar) fields)) $ \(name, nfields) ->
    for_ nfields $ \(MkNamelessField pos _) ->
      if name `Set.member` sections
        then parseFailure pos $ "'" ++ fromUTF8BS name ++ "' is a stanza, not a field. Remove the trailing ':' to parse a stanza."
        else parseWarning pos PWTUnknownField $ "Unknown field: " ++ show name
  fieldGrammarParser grammar v fields

fieldGrammarKnownFieldList :: ParsecFieldGrammar s a -> [FieldName]
fieldGrammarKnownFieldList = Set.toList . fieldGrammarKnownFields

instance Applicative (ParsecFieldGrammar s) where
  pure x = ParsecFG mempty mempty (\_ _ -> pure (mempty, x))
  {-# INLINE pure #-}

  ParsecFG f f' f'' <*> ParsecFG x x' x'' =
    ParsecFG
      (mappend f x)
      (mappend f' x')
      (\v fields -> liftA2 (<*>) (f'' v fields) (x'' v fields))
  {-# INLINE (<*>) #-}

warnMultipleSingularFields :: FieldName -> [NamelessField Position] -> ParseResult src ()
warnMultipleSingularFields _ [] = pure ()
warnMultipleSingularFields fn (x : xs) = do
  let pos = namelessFieldAnn x
      poss = map namelessFieldAnn xs
  parseWarning pos PWTMultipleSingularField $
    "The field " <> show fn <> " is specified more than once at positions " ++ intercalate ", " (map showPos (pos : poss))

instance FieldGrammar ExactParsec ParsecFieldGrammar where
  withScope :: Markable ns => ns -> ParsecFieldGrammar s a -> ParsecFieldGrammar s a
  withScope ns (ParsecFG s s' parser) =
    ParsecFG s s' $ \v fs -> do
      -- Run the inner parser and mark its annotation
      (t, x) <- parser v fs
      let t' = markTriviaTree ns t
      pure (t', x)

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

      parseOne v (MkNamelessField pos fls) =
        fmap (unpack' _pack) <$> runFieldParser pos exactParsec v fls

  booleanFieldDef fn _extract def = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> (pure . pure) def
        Just [] -> (pure . pure) def
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (MkNamelessField pos fls) = runFieldParser pos exactParsec v fls

  optionalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> (pure . pure) Nothing
        Just [] -> (pure . pure) Nothing
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (MkNamelessField pos fls)
        | null fls = (pure . pure) Nothing
        | otherwise = fmap (Just . unpack' _pack) <$> runFieldParser pos exactParsec v fls

  optionalFieldDefAla fn _pack _extract def = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> (pure . pure) def
        Just [] -> (pure . pure) def
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (MkNamelessField pos fls)
        | null fls = (pure . pure) def
        | otherwise = fmap (unpack' _pack) <$> runFieldParser pos exactParsec v fls

  freeTextField fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> (pure . pure) Nothing
        Just [] -> (pure . pure) Nothing
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (MkNamelessField pos fls)
        | null fls = (pure . pure) Nothing
        | v >= CabalSpecV3_0 = (pure . pure) (Just (fieldlinesToFreeText3 pos fls))
        | otherwise = (pure . pure) (Just (fieldlinesToFreeText fls))

  freeTextFieldDef fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> (pure . pure) ""
        Just [] -> (pure . pure) ""
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (MkNamelessField pos fls)
        | null fls = (pure . pure) ""
        | v >= CabalSpecV3_0 = (pure . pure) (fieldlinesToFreeText3 pos fls)
        | otherwise = (pure . pure) (fieldlinesToFreeText fls)

  -- freeTextFieldDefST = defaultFreeTextFieldDefST
  freeTextFieldDefST fn _ = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> (pure . pure) mempty
        Just [] -> (pure . pure) mempty
        Just [x] -> parseOne v x
        Just xs@(_ : y : ys) -> do
          warnMultipleSingularFields fn xs
          NE.last <$> traverse (parseOne v) (y :| ys)

      parseOne v (MkNamelessField pos fls) = case fls of
        [] -> (pure . pure) mempty
        [FieldLine _ bs] -> (pure . pure) (ShortText.unsafeFromUTF8BS bs)
        _
          | v >= CabalSpecV3_0 -> (pure . pure) (ShortText.toShortText $ fieldlinesToFreeText3 pos fls)
          | otherwise -> (pure . pure) (ShortText.toShortText $ fieldlinesToFreeText fls)

  monoidalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
    where
      parser v fields = case Map.lookup fn fields of
        Nothing -> (pure . pure) mempty
        Just xs -> do
          (t, x) <- foldMap (unpack' _pack <$>) <$> traverse (uncurry (parseOne v)) (zip xs [1 ..])
          let t' = markTriviaTree fn t
          pure (t', x)

      parseOne v (MkNamelessField pos fls) n = do
        (t, x) <- runFieldParser pos exactParsec v fls
        let t' = TriviaTree [ExactPosition pos] mempty <> fromNamedTrivia x [FieldNth n] <> t
        pure (t', x)

  prefixedFields fnPfx _extract = ParsecFG mempty (Set.singleton fnPfx) (\_ fs -> pure (mempty, parser fs))
    where
      parser :: Fields Position -> [(String, String)]
      parser values = reorder $ concatMap convert $ filter match $ Map.toList values

      match (fn, _) = fnPfx `BS.isPrefixOf` fn
      convert (fn, fields) =
        [ (pos, (fromUTF8BS fn, trim $ fromUTF8BS $ fieldlinesToBS fls))
        | MkNamelessField pos fls <- fields
        ]
      -- hack: recover the order of prefixed fields
      reorder = map snd . sortBy (comparing fst)

  availableSince vs def (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v values
        | v >= vs = parser v values
        | otherwise = do
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList unknownFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField pos _) ->
                parseWarning pos PWTUnknownField $
                  "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ ". This field will be ignored."

            (pure . pure) def

  availableSinceWarn vs (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v values
        | v >= vs = parser v values
        | otherwise = do
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList unknownFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField pos _) ->
                parseWarning pos PWTUnknownField $
                  "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ "."

            parser v values

  -- todo we know about this field
  deprecatedSince vs msg (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v values
        | v >= vs = do
            let deprecatedFields = Map.intersection values $ Map.fromSet (const ()) names
            for_ (Map.toList deprecatedFields) $ \(name, fields) ->
              for_ fields $ \(MkNamelessField pos _) ->
                parseWarning pos PWTDeprecatedField $
                  "The field " <> show name <> " is deprecated in the Cabal specification version " ++ showCabalSpecVersion vs ++ ". " ++ msg

            parser v values
        | otherwise = parser v values

  removedIn vs msg (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
    where
      parser' v values
        | v >= vs = do
            let msg' = if null msg then "" else ' ' : msg
            let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
            let namePos =
                  [ (name, pos)
                  | (name, fields) <- Map.toList unknownFields
                  , MkNamelessField pos _ <- fields
                  ]

            let makeMsg name = "The field " <> show name <> " is removed in the Cabal specification version " ++ showCabalSpecVersion vs ++ "." ++ msg'

            case namePos of
              -- no fields => proceed (with empty values, to be sure)
              [] -> parser v mempty
              -- if there's single field: fail fatally with it
              ((name, pos) : rest) -> do
                for_ rest $ \(name', pos') -> parseFailure pos' $ makeMsg name'
                parseFatalFailure pos $ makeMsg name
        | otherwise = parser v values

  knownField fn = ParsecFG (Set.singleton fn) Set.empty (\_ _ -> pure (mempty, ()))

  hiddenField = id

-------------------------------------------------------------------------------
-- Parsec
-------------------------------------------------------------------------------

runFieldParser' :: [Position] -> ParsecParser a -> CabalSpecVersion -> FieldLineStream -> ParseResult src a
runFieldParser' inputPoss p v str = case P.runParser p' [] "<field>" str of
  Right (pok, warns) -> do
    traverse_ (\(PWarning t pos w) -> parseWarning (mapPosition pos) t w) warns
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

runFieldParser :: Position -> ParsecParser a -> CabalSpecVersion -> [FieldLine Position] -> ParseResult src a
runFieldParser pp p v ls = runFieldParser' poss p v (fieldLinesToStream ls)
  where
    poss = map (\(FieldLine pos _) -> pos) ls ++ [pp] -- add "default" position

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
fieldLinesToStream [] = fieldLineStreamEnd
fieldLinesToStream [FieldLine pos bs] = FLSLast bs (Just pos)
fieldLinesToStream (FieldLine pos bs : fs) = FLSCons bs (Just pos) (fieldLinesToStream fs)
