{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Fields.Transform where

import qualified Text.Parsec as P

import Distribution.FieldGrammar.Parsec (joinFieldLines, splitFieldLines)
import Distribution.Fields.Field
import Distribution.Parsec.Position
import Distribution.Pretty

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import GHC.Generics
import Data.Coerce
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either
import Data.Kind
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Distribution.Annotation
import Distribution.FieldGrammar.Newtypes
import Distribution.Parsec
import Distribution.Parsec.FieldLineStream

import Debug.Trace
import qualified Distribution.Parsec.Position.Lens as L
import qualified Distribution.Fields.Field.Lens as L
import qualified Distribution.Compat.Lens as L

-- TODO(leana8959): possible failures:
-- - focus not found, warning?
-- - modification failed -> error
-- - parse failure -> error
-- - not identpodent -> error
-- - nothing changed and we expect things to change


data EditError
  = ExpectChanges -- TODO(leana8959): not very helpful, we dont' know what didn't change exactly
  | ParseFailed
  deriving (Generic)

data AddConfig = AddStart | AddEnd
data RemoveConfig = RemoveAll | RemoveFirst
data ModifyConfig = ModifyFirst | ModifyLast

-- TODO(leana8959): do we need to provide all fields in the predicate
type MatchField ann = Name ann -> [FieldLine ann] -> Bool
type MatchSection ann = Name ann -> [SectionArg ann] -> [Field ann] -> Bool

-- TODO(leana8959): add a state for position

data EditResult a
  = EditOk a
  | EditUnchanged a
  | EditErr EditError
    -- ^ a case where you might want to fallback to other edits, because apparently what you did did nothing.
  deriving (Functor, Generic)

-- | Build an 'EditResult' given the previous value
mkEditResult :: Eq a => a -> a -> EditResult a
mkEditResult old new
  | old == new = EditUnchanged new
  | otherwise = EditOk new

instance Applicative EditResult where
  pure x = EditOk x
  liftA2 f = \cases
    (EditOk x) (EditOk y) -> EditOk (f x y)

    -- As long as something is changed, it's ok.
    (EditOk x) (EditUnchanged y) -> EditOk (f x y)
    (EditUnchanged y) (EditOk x) -> EditOk (f y x)
    (EditUnchanged u) (EditUnchanged v) -> EditUnchanged (f u v)

    -- Failure takes precedence
    _ (EditErr err) -> EditErr err
    (EditErr err) _ -> EditErr err

instance Monad EditResult where
  (EditOk x) >>= f = f x
  (EditUnchanged u) >>= f = f u
  (EditErr err) >>= _ = EditErr err

-- We don't have alternative instance because empty doesnt' make sense.
-- Either we have to conjure up a Unchanged out of thin air
-- Or we need to fail with empty.
orFallback' :: EditResult a -> EditResult a -> EditResult a
orFallback' = \cases
  (EditOk x) _ -> EditOk x
  (EditUnchanged _) y -> y
  (EditErr _) y -> y

-- TODO(leana8959): add a case for adding a section

-- Different modification strategies
modifyField
  :: ModifyConfig
  -> MatchField (WithComments Position) -- ^ Match a given field
  -> ([FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)])
  -> Edit [Field (WithComments Position)]
modifyField mc match modifyFieldLines = Edit $ case mc of
  ModifyFirst -> \fs -> mapFirstThen doModify doShift fs
  ModifyLast -> mapLast doModify
  where
    doModify (Field colonPos fname fls) | match fname fls = Field colonPos fname <$> fls'
      where
        fls' = (mkEditResult <*> modifyFieldLines) fls
    doModify x = EditUnchanged x

    doShift (old, new) fd =
        let (_, oldEnd) = fieldRowRange old
            (_, newEnd) = fieldRowRange new
            lineShift = (newEnd - oldEnd) `max` 0
        in  offsetFieldRow lineShift fd

fieldsRowRange :: L.HasPosition ann => NonEmpty (Field ann) -> (Int, Int)
fieldsRowRange = finalize . fmap fieldRowRange
  where
    finalize ranges = (fst (NE.head ranges), snd (NE.last ranges))

fieldRowRange :: L.HasPosition ann => Field ann -> (Int, Int)
fieldRowRange (Field _colonPos fname fls) =
  -- TODO(leana8959): can colon be on a different row?
  let nameRow = L.view L.positionRow (nameAnn fname)
      maybeLastFieldLinePos = L.view L.positionRow . fieldLineAnn . NE.last <$> NE.nonEmpty fls
  in  (nameRow, fromMaybe nameRow maybeLastFieldLinePos)
  -- TODO(leana8959): can sargs be different lines?
fieldRowRange (Section sname _sargs fs) =
  let nameRow = L.view L.positionRow (nameAnn sname)
      bodyEnd = snd . fieldsRowRange <$> NE.nonEmpty fs
  in  (nameRow, fromMaybe nameRow bodyEnd)

-- | Compute the ending position of a field based on its range.
afterFieldEndPosition :: L.HasPosition ann => Field ann -> Position
afterFieldEndPosition f =
  let (_, endRow) = fieldRowRange f
  in  Position (endRow + 1 {- next line -}) 1

offsetFieldRow :: L.HasPosition ann => Int -> Field ann -> Field ann
offsetFieldRow n = \case
  (Field colonPos fname fls) -> Field (incrementRowN colonPos) (fmap incrementRowN fname) (map (fmap incrementRowN) fls)
  (Section sname sargs fs) -> Section (fmap incrementRowN sname) (map (fmap incrementRowN) sargs) (map (fmap incrementRowN) fs)
  where
    incrementRowN :: L.HasPosition ann => ann -> ann
    incrementRowN = L.over L.positionRow (+n)

addField
  :: AddConfig
  -> Name (WithComments ())
  -> [FieldLine (WithComments ())]
  -> Edit [Field (WithComments Position)]
addField ac name fls = Edit $ case ac of
  AddStart -> \case
    [] -> EditOk [fst (mkNewFieldAt onePos)]
    fs@(f : _) ->
      let (newField, newFieldHeight) = mkNewFieldAt (L.view L.position (fieldAnn f))
          fs' = offsetFieldRow newFieldHeight <$> fs
      in  EditOk (newField : fs')
  AddEnd ->
    let go = \case
          [] -> EditOk [fst (mkNewFieldAt onePos)]
          [f] ->
            let (newField, _) = mkNewFieldAt (afterFieldEndPosition f)
            in  EditOk [f, newField]
          (f : fs) -> (f :) <$> go fs
    in go
  where
    mkNewFieldAt :: Position -> (Field (WithComments Position), Int)
    mkNewFieldAt pos =
      let newField = Field (L.set L.positionCol (nameLen + 1) pos) (fmap (pos <$) name) fls'
      in  (newField, 1 + flsHeight)
      where
        nameLen = BS8.length (getName name)
        startRow = positionRow pos
        -- TODO(leana8959): how do we know the indentation?
        startCol = positionCol pos + 2

        fls' :: [FieldLine (WithComments Position)]
        (fls', flsHeight) = case fls of
          [] -> ([], 0)
          [fl] -> ([fmap (Position startRow {- start on same line -} (positionCol pos + 2) <$) fl], 0)
          _ ->
            ( zipWith (\fl row -> fmap (Position row startCol <$) fl) fls [startRow + 1 {- start on next line -}..]
            , length fls )

removeField
  :: RemoveConfig
  -> MatchField (WithComments Position)
  -> Edit [Field (WithComments Position)]
removeField rc match = Edit $ case rc of
  RemoveAll -> mkEditResult <*> filter p
  RemoveFirst -> mkEditResult <*> filterOne p
  where
    p (Field _ name fls) = not (match name fls)
    p _ = True

modifySection
  :: ModifyConfig
  -> MatchSection (WithComments Position) -- ^ Match a given section
  -> ([SectionArg (WithComments Position)] -> [SectionArg (WithComments Position)]) -- ^ Transform the section args
  -> Edit [Field (WithComments Position)] -- ^ Transform inner fields
  -> Edit [Field (WithComments Position)]
modifySection mc match modifySectionArgs modifyFields = Edit $ case mc of
  ModifyFirst -> mapFirstThen doModify doShift
  ModifyLast -> mapLast doModify
  where
    doModify (Section sname sargs fs) | match sname sargs fs =
      let sargs' = (mkEditResult <*> modifySectionArgs) sargs
          fs' = runEdit modifyFields fs
       in Section sname <$> sargs' <*> fs'
    doModify x = EditUnchanged x

    doShift (old, new) fd =
        let (_, oldEnd) = fieldRowRange old
            (_, newEnd) = fieldRowRange new
            lineShift = (newEnd - oldEnd) `max` 0
        in  offsetFieldRow lineShift fd

-- | Fallback if something is unchanged.
orFallback :: Edit a -> Edit a -> Edit a
orFallback (Edit x) (Edit y) = Edit (\input -> x input `orFallback'` y input)

infixl 4 `orFallback`

-- | If up to this point things are still unchanged, make it an error and stop here.
failIfUnchanged :: Edit a -> Edit a
failIfUnchanged (Edit f) = Edit $ \input -> case f input of
  EditUnchanged {} -> EditErr ExpectChanges
  other -> other

-- | The product operator should deal with the positioning chaining
andThen :: Edit a -> Edit a -> Edit a
andThen (Edit x) (Edit y) = Edit (x >=> y)

infixl 5 `andThen`

-- note to self: ModifySection will be hella useful when it comes to changing conditions (if sections)

-- | Filter but only drop one that doesn't fit the predicate.
filterOne :: (a -> Bool) -> [a] -> [a]
filterOne _ [] = []
filterOne f (x : xs)
  | f x = x : filterOne f xs
  | otherwise = xs

-- | Map until the first mapping function succeeds (commited changes).
mapFirstThen
  :: (a -> EditResult a)
  -- ^ edit a
  -> ((a, a) -> a -> a)
  -- ^ what to do after the first edit given before and after
  -> [a]
  -> EditResult [a]
mapFirstThen _ _ [] = EditUnchanged []
mapFirstThen f g (x : xs) = case f x of
  EditOk x' -> EditOk (x' : map (g (x, x')) xs)
  EditUnchanged x' -> (x' :) <$> mapFirstThen f g xs
  EditErr err -> EditErr err

mapFirst :: (a -> EditResult a) -> [a] -> EditResult [a]
mapFirst f = mapFirstThen f (const id)

mapLast :: (a -> EditResult a) -> [a] -> EditResult [a]
mapLast f = fmap reverse . mapFirst f . reverse

data EditingError = ParserError P.ParseError {- not yet used -} | PrinterError
  deriving (Show)

-- | Reified function to facilitate chaining
-- This can't be a functor (so there's no applicative nor alternative), I think there are better ways to do this.
newtype Edit a = Edit { runEdit :: a -> EditResult a }

-- TODO(leana8959): what do we do when there are no existing value?
-- currently we do nothing.
modifyValueAtomBSAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> (BS.ByteString -> BS.ByteString)
modifyValueAtomBSAla transformA bs0 =
  let parsed =
        (coerce @b @a)
          . fromRight (error "modifyValueAtomAla failed to parse")
          . runParsecParser (parsec @b) "<modifyValueAtomAla>"
          . fieldLineStreamFromBS
          $ bs0

      transformed = transformA parsed

      bs = maybe bs0 (BS8.pack . show . pretty @b . coerce @a @b) transformed
   in bs

-- | Build a @[FieldLine Position]@ modification function given a function @a -> a@, parsed as @b@.
modifyValueAtomAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> ([FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)])
modifyValueAtomAla transformA fls = case joinFieldLines <$> NE.nonEmpty fls of
  -- no original data
  Nothing -> fls
  Just (FieldLine ann0 bs0) ->
    let bs = modifyValueAtomBSAla @b @a transformA bs0
        fls' = splitFieldLines (WithComments [] zeroPos) (FieldLine ann0 bs)
     in fls'

-- | The position is (1, 1)-indexed. The second element of the pair starts at the position.
splitBSAtPosition :: Position -> BS.ByteString -> (BS.ByteString, BS.ByteString)
splitBSAtPosition (Position row col) bs = case splitAt (row - 1) (BS8.lines bs) of
  (preLines, []) -> (BS8.unlines preLines, "\n") -- TODO(leana8959): it is useful, but is it lawful?
  (preLines, l : postLines) ->
    let (ll, lr) = BS8.splitAt (col - 1) l
     in (BS8.unlines preLines <> ll, BS8.unlines (lr : postLines))

substituteSubBSAt :: SrcSpan -> BS.ByteString -> BS.ByteString -> BS.ByteString
substituteSubBSAt (SrcSpan begin end) originalBS newBS =
  let (preBS, _) = splitBSAtPosition begin originalBS
      (_, postBS) = splitBSAtPosition end originalBS
      bs' = preBS <> newBS <> postBS
   in bs'

appendNewLines :: SrcSpan -> BS.ByteString -> BS.ByteString
appendNewLines (SrcSpan begin end) =
  let rowBegin = positionRow begin
      rowEnd = positionRow end
      rowDiff = rowEnd - rowBegin
   in if rowDiff > 1 then (<> BS8.replicate rowDiff '\n') else id

-- | Primitive function that edits the joined ByteString of a Field
modifyValueListBS
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Pretty b
     , Parsec (List sep (Located b) (Located a))
     )
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> (BS.ByteString -> BS.ByteString)
modifyValueListBS transformA bs0 =
  let parsed =
        coerce @_ @[Located a]
          . fromRight (error "modifyValueList failed to parse")
          -- NOTE(leana8959): Eh, the parser doesn't like leading spaces.
          . runParsecParser (liftParsec P.spaces *> parsec @(List sep (Located b) (Located a))) "<modifyValueList>"
          -- NOTE(leana8959): do we need the world's position here
          . fieldLineStreamFromBS
          $ bs0

      transformed :: [Located (Maybe a)] = (map . fmap) transformA parsed

      -- From back to front (avoid drifting), generate a list of (source range, replacement string)
      editsWithinList =
        sortBySrcSpanDes transformed >>= \case
          MkLocated _ Nothing -> []
          MkLocated spn (Just newItem) -> [(spn, BS8.pack $ show $ pretty @b $ coerce @a @b newItem)]

      -- TODO(leana8959): think about performance later. Rope?
      performEditsWithinList = foldl' go
        where
          -- If the original snippet spans more than one line, we append the vertical spacing back by counting lines.
          -- This is because each Parsec instance eats the trailing spaces.
          go oldBS (spn, bs) = substituteSubBSAt spn oldBS (appendNewLines spn bs)

      printed = performEditsWithinList bs0 editsWithinList
   in printed

-- TODO(leana8959): make modification function partial

-- | Build a @[FieldLine Position]@ modification function given a function @a -> Maybe a@, parsed as @List sep b a@.
modifyValueList
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Pretty b
     , Parsec (List sep (Located b) (Located a))
     )
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> ([FieldLine (WithComments Position)] -> [FieldLine (WithComments Position)])
modifyValueList transformA fls = case joinFieldLines <$> NE.nonEmpty fls of
  -- no original data
  Nothing -> fls
  Just (FieldLine ann0 bs0) ->
    let bs = modifyValueListBS @sep @b @a transformA bs0
        fls' = splitFieldLines (WithComments [] zeroPos) (FieldLine ann0 bs)
     in fls'

prependValueListBS
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Sep sep
     , Pretty b
     , Parsec (List sep (Located b) (Located a))
     )
  => a
  -- ^ Nothing prevents a new render.
  -> (BS.ByteString -> BS.ByteString)
prependValueListBS newItem bs0 =
  let parsed =
        coerce @_ @[Located a]
          . fromRight (error "modifyValueList failed to parse")
          -- NOTE(leana8959): Eh, the parser doesn't like leading spaces.
          . runParsecParser (liftParsec P.spaces *> parsec @(List sep (Located b) (Located a))) "<modifyValueList>"
          -- NOTE(leana8959): do we need the world's position here
          . fieldLineStreamFromBS
          $ bs0

      newItemBS = BS8.pack $ show $ pretty @b $ coerce @a @b newItem

      printed = case parsed of
        [] -> newItemBS
        (MkLocated (SrcSpan begin _) _ : _) ->
          let sep = Proxy :: Proxy sep
              (preOldFirstBS, _) = splitBSAtPosition begin bs0
              hasLeadingSep = BS8.any (isSeparator sep) preOldFirstBS
              newSep = sepToChar sep
           in if hasLeadingSep
                then newItemBS <> bs0
                else newItemBS <> newSep <> bs0
   in printed
