{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- slighly higher-level API, so we can process unspecified fields,
-- to report unknown fields and save custom @x-fields@.
--
module Distribution.FieldGrammar.Parsec (
    ParsecFieldGrammar,
    parseFieldGrammar,
    fieldGrammarKnownFieldList,
    -- * Auxiliary
    Fields,
    NamelessField (..),
    namelessFieldAnn,
    Section (..),
    runFieldParser,
    runFieldParser',
    )  where

import Data.List                   (dropWhileEnd)
import Data.Ord                    (comparing)
import Data.Set                    (Set)
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude
import Distribution.Simple.Utils   (fromUTF8BS)
import Prelude ()

import qualified Data.ByteString   as BS
import qualified Data.Map.Strict   as Map
import qualified Data.Set          as Set
import qualified Text.Parsec       as P
import qualified Text.Parsec.Error as P

import Distribution.CabalSpecVersion
import Distribution.FieldGrammar.Class
import Distribution.Parsec.Class
import Distribution.Parsec.Common
import Distribution.Parsec.Field
import Distribution.Parsec.FieldLineStream
import Distribution.Parsec.ParseResult

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
    { fieldGrammarKnownFields   :: !(Set FieldName)
    , fieldGrammarKnownPrefixes :: !(Set FieldName)
    , fieldGrammarParser        :: !(CabalSpecVersion -> Fields Position -> ParseResult a)
    }
  deriving (Functor)

parseFieldGrammar :: CabalSpecVersion -> Fields Position -> ParsecFieldGrammar s a -> ParseResult a
parseFieldGrammar v fields grammar = do
    for_ (Map.toList (Map.filterWithKey isUnknownField fields)) $ \(name, nfields) ->
        for_ nfields $ \(MkNamelessField pos _) ->
            parseWarning pos PWTUnknownField $ "Unknown field: " ++ show name
            -- TODO: fields allowed in this section

    -- parse
    fieldGrammarParser grammar v fields

  where
    isUnknownField k _ = not $
        k `Set.member` fieldGrammarKnownFields grammar
        || any (`BS.isPrefixOf` k) (fieldGrammarKnownPrefixes grammar)

fieldGrammarKnownFieldList :: ParsecFieldGrammar s a -> [FieldName]
fieldGrammarKnownFieldList = Set.toList . fieldGrammarKnownFields

instance Applicative (ParsecFieldGrammar s) where
    pure x = ParsecFG mempty mempty (\_ _  -> pure x)
    {-# INLINE pure  #-}

    ParsecFG f f' f'' <*> ParsecFG x x' x'' = ParsecFG
        (mappend f x)
        (mappend f' x')
        (\v fields -> f'' v fields <*> x'' v fields)
    {-# INLINE (<*>) #-}

warnMultipleSingularFields :: FieldName -> [NamelessField Position] -> ParseResult ()
warnMultipleSingularFields _ [] = pure ()
warnMultipleSingularFields fn (x : xs) = do
    let pos  = namelessFieldAnn x
        poss = map namelessFieldAnn xs
    parseWarning pos PWTMultipleSingularField $
        "The field " <> show fn <> " is specified more than once at positions " ++ intercalate ", " (map showPos (pos : poss))

instance FieldGrammar ParsecFieldGrammar where
    blurFieldGrammar _ (ParsecFG s s' parser) = ParsecFG s s' parser

    uniqueFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser v fields = case Map.lookup fn fields of
            Nothing -> parseFatalFailure zeroPos $ show fn ++ " field missing"
            Just [] -> parseFatalFailure zeroPos $ show fn ++ " field missing"
            Just [x] -> parseOne v x
            Just xs -> do
                warnMultipleSingularFields fn xs
                last <$> traverse (parseOne v) xs

        parseOne v (MkNamelessField pos fls) =
            unpack' _pack <$> runFieldParser pos parsec v fls

    booleanFieldDef fn _extract def = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser v fields = case Map.lookup fn fields of
            Nothing  -> pure def
            Just []  -> pure def
            Just [x] -> parseOne v x
            Just xs  -> do
                warnMultipleSingularFields fn xs
                last <$> traverse (parseOne v) xs

        parseOne v (MkNamelessField pos fls) = runFieldParser pos parsec v fls

    optionalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser v fields = case Map.lookup fn fields of
            Nothing  -> pure Nothing
            Just []  -> pure Nothing
            Just [x] -> parseOne v x
            Just xs  -> do
                warnMultipleSingularFields fn xs
                last <$> traverse (parseOne v) xs

        parseOne v (MkNamelessField pos fls)
            | null fls  = pure Nothing
            | otherwise = Just . unpack' _pack <$> runFieldParser pos parsec v fls

    optionalFieldDefAla fn _pack _extract def = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser v fields = case Map.lookup fn fields of
            Nothing  -> pure def
            Just []  -> pure def
            Just [x] -> parseOne v x
            Just xs  -> do
                warnMultipleSingularFields fn xs
                last <$> traverse (parseOne v) xs

        parseOne v (MkNamelessField pos fls)
            | null fls  = pure def
            | otherwise = unpack' _pack <$> runFieldParser pos parsec v fls

    monoidalFieldAla fn _pack _extract = ParsecFG (Set.singleton fn) Set.empty parser
      where
        parser v fields = case Map.lookup fn fields of
            Nothing -> pure mempty
            Just xs -> foldMap (unpack' _pack) <$> traverse (parseOne v) xs

        parseOne v (MkNamelessField pos fls) = runFieldParser pos parsec v fls

    prefixedFields fnPfx _extract = ParsecFG mempty (Set.singleton fnPfx) (\_ fs -> pure (parser fs))
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
        trim :: String -> String
        trim = dropWhile isSpace . dropWhileEnd isSpace

    availableSince vs def (ParsecFG names prefixes parser) = ParsecFG names prefixes parser'
      where
        parser' v values
            | v >= vs = parser v values
            | otherwise = do
                let unknownFields = Map.intersection values $ Map.fromSet (const ()) names
                for_ (Map.toList unknownFields) $ \(name, fields) ->
                    for_ fields $ \(MkNamelessField pos _) ->
                        parseWarning pos PWTUnknownField $
                            "The field " <> show name <> " is available only since the Cabal specification version " ++ showCabalSpecVersion vs ++ "."

                pure def

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

    removedIn vs msg (ParsecFG names prefixes parser) = ParsecFG names prefixes parser' where
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

    knownField fn = ParsecFG (Set.singleton fn) Set.empty (\_ _ -> pure ())

    hiddenField = id

-------------------------------------------------------------------------------
-- Parsec
-------------------------------------------------------------------------------

runFieldParser' :: [Position] -> ParsecParser a -> CabalSpecVersion -> FieldLineStream -> ParseResult a
runFieldParser' inputPoss p v str = case P.runParser p' [] "<field>" str of
    Right (pok, ws) -> do
        traverse_ (\(PWarning t pos w) -> parseWarning (mapPosition pos) t w) ws
        pure pok
    Left err        -> do
        let ppos = P.errorPos err
        let epos = mapPosition $ Position (P.sourceLine ppos) (P.sourceColumn ppos)

        let msg = P.showErrorMessages
                "or" "unknown parse error" "expecting" "unexpected" "end of input"
                (P.errorMessages err)
        parseFatalFailure epos $ msg ++ "\n"
  where
    p' = (,) <$ P.spaces <*> unPP p v <* P.spaces <* P.eof <*> P.getState

    -- Positions start from 1:1, not 0:0
    mapPosition (Position prow pcol) = go (prow - 1) inputPoss where
        go _ []                            = zeroPos
        go _ [Position row col]            = Position row (col + pcol - 1)
        go n (Position row col:_) | n <= 0 = Position row (col + pcol - 1)
        go n (_:ps)                        = go (n - 1) ps

runFieldParser :: Position -> ParsecParser a -> CabalSpecVersion -> [FieldLine Position] -> ParseResult a
runFieldParser pp p v ls = runFieldParser' poss p v (fieldLinesToStream ls)
  where
    poss = map (\(FieldLine pos _) -> pos) ls ++ [pp] -- add "default" position

fieldlinesToBS :: [FieldLine ann] -> BS.ByteString
fieldlinesToBS = BS.intercalate "\n" . map (\(FieldLine _ bs) -> bs)
