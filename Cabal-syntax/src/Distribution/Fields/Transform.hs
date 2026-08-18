{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Fields.Transform
  (
    Edit (..)
  , EditError (..)
  , EditResult (..)
  , MatchField
  , MatchSection

    -- * Addition
  , AddConfig (..)
  , addField

    -- * Removal
  , RemoveConfig (..)
  , removeField

    -- * Modification
  , ModifyConfig (..)
  , modifyField
  , modifySection

    -- * Control flow
  , failIfUnchanged
  , andThen
  , orFallback

    -- * Typed modification functions
  , modifyValueAtomBSAla
  , modifyValueAtomAla
  , modifyValueListBS
  , modifyValueList
  , prependValueListBS
  -- , prependValueList

  -- TODO: move to an internal module
  -- * Internal
  , substituteSubBSAt
  , splitBSAtPosition
  )
  where

import qualified Text.Parsec as P

import Distribution.FieldGrammar.Parsec
  ( joinFieldLines, splitFieldLines, extractComments, removeComments
  , interleaveComments
  )
import Distribution.Fields.Field
import Distribution.Parsec.Position
import Distribution.Pretty
import Distribution.Utils.Generic

import Data.Functor ((<&>))
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

import qualified Text.Parsec as P

import Debug.Trace
import qualified Distribution.Parsec.Position.Lens as L
import qualified Distribution.Fields.Field.Lens as L
import qualified Distribution.Compat.Lens as L

data EditError
  = ExpectChanges
  -- TODO(leana8959): use the label
  | ParseFailed P.ParseError
  deriving (Eq, Generic)

data AddConfig = AddStart | AddEnd
data RemoveConfig = RemoveAll | RemoveFirst
data ModifyConfig = ModifyFirst | ModifyLast

-- TODO(leana8959): Add a label to indicate what didn't match, or what matched and didn't change.
type MatchField ann = Name ann -> [FieldLine ann] -> Bool
type MatchSection ann = Name ann -> [SectionArg ann] -> [Field ann] -> Bool

data EditResult a
  = EditOk a
  | EditUnchanged a
  | EditErr EditError
    -- ^ a case where you might want to fallback to other edits, because apparently what you did did nothing.
  deriving (Eq, Functor, Generic)

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

-- We don't have alternative instance because empty doesn't make sense.
-- Either we have to conjure up a Unchanged out of thin air;
-- or we need to fail with empty.
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
  -> ([FieldLine (WithComments Position)] -> EditResult [FieldLine (WithComments Position)])
  -> Edit [Field (WithComments Position)]
modifyField mc match modifyFieldLines = Edit $ case mc of
  ModifyFirst -> \fs -> mapFirstThen doModify doShift fs
  ModifyLast -> mapLast doModify
  where
    doModify (Field colonPos fname fls) | match fname fls = Field colonPos fname <$> fls'
      where
        fls' = modifyFieldLines fls
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
  let nameRow = L.view L.positionRow (nameAnn fname)
      maybeLastFieldLinePos = L.view L.positionRow . fieldLineAnn . NE.last <$> NE.nonEmpty fls
  in  (nameRow, fromMaybe nameRow maybeLastFieldLinePos)
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

-- TODO(leana8959): rewrite this by simply asking the user to provide some comments and we put it at a fixed position.
-- The fact that WithComments holds comments with positions makes it very hard to reason with.
addField
  :: AddConfig
  -> Name ()
  -> [BS.ByteString] -- ^ Comments to the 'Name'
  -> [FieldLine ()]
  -> [BS.ByteString] -- ^ Comments to the 'FieldLine's
  -> Edit [Field (WithComments Position)]
addField ac name nameCmts fls flsCmts = Edit $ case ac of
  AddStart -> \case
    [] -> EditOk [fst (mkFieldAt' onePos)]
    fs@(f : _) ->
      let (newField, newFieldHeight) = mkFieldAt' (L.view L.position (fieldAnn f))
          fs' = offsetFieldRow newFieldHeight <$> fs
      in  EditOk (newField : fs')
  AddEnd ->
    let go [] = EditOk [fst (mkFieldAt' onePos)]
        go [f] =
            let (newField, _) = mkFieldAt' (afterFieldEndPosition f)
            in  EditOk [f, newField]
        go (f : fs) = (f :) <$> go fs
    in go
  where
    mkFieldAt' = mkFieldAt name nameCmts fls flsCmts

-- | Create a new field at a position, along with its height.
--   Comments are plain strings without @--@ prefix.
mkFieldAt
  :: Name ()
  -> [BS.ByteString] -- ^ Comments to the 'Name'
  -> [FieldLine ()]
  -> [BS.ByteString] -- ^ Comments to the 'FieldLine's
  -> Position
  -> (Field (WithComments Position), Int)
mkFieldAt name nameCmts fls flsCmts pos =
  let -- If there are no body, we attach the fieldlines's comments to the field.
      cmtsAboveFieldBS = if null fls then nameCmts <> flsCmts else nameCmts

      -- All content is aligned to this column.
      col0 = positionCol pos
      -- TODO(leana8959): read indentation from the config
      indentedCol = col0 + 2

      mkCmtAt row col bs = Comment ("-- " <> bs) (Position row col0)
      cmtsAboveField = zipWith (\bs row -> mkCmtAt row col0 bs) cmtsAboveFieldBS [ positionRow pos .. ]

      namePosition = maybe pos (\(Comment _ p) -> retPos p) (safeLast cmtsAboveField)
      nameWithAnn = (WithComments cmtsAboveField namePosition) <$ name

      colonPos = incPos (BS8.length (getName name)) namePosition

      (flsWithAnn, flsHeight) = case fls of
        [] -> ([], 0)
        (fl : fls') ->
          let cmtsAboveFls = zipWith (\bs row -> mkCmtAt row indentedCol bs) flsCmts [ positionRow pos .. ]
              fieldLineStartPos = maybe pos (\(Comment _ p) -> retPos p) (safeLast cmtsAboveFls)

              fl' :: FieldLine (WithComments Position)
              fl' = WithComments cmtsAboveFls fieldLineStartPos <$ fl
              fieldLineFollowingPos = retPos fieldLineStartPos

              fls'' :: [FieldLine (WithComments Position)]
              fls'' = zipWith (\fl row -> WithComments mempty (Position row indentedCol) <$ fl) fls' [ positionRow fieldLineFollowingPos ..]
          in  (fl' : fls'', length cmtsAboveFls + 1 + length fls'')

      field = Field colonPos nameWithAnn flsWithAnn
      totalHeight =
          length cmtsAboveField
          + 1 -- field
          + flsHeight -- includes inner comments
  in  (field, totalHeight)

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

-- TODO(leana8959): add an argument that passes in various edit contexts, like a reader monad.
newtype Edit a = Edit { runEdit :: a -> EditResult a }

modifyValueAtomBSAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> (BS.ByteString -> EditResult BS.ByteString)
modifyValueAtomBSAla transformA bs0 =
  let parsed =
        fmap (coerce @b @a)
          . runParsecParser (parsec @b) "<modifyValueAtomAla>"
          . fieldLineStreamFromBS
          $ bs0
  in case parsed of
    Left err -> EditErr (ParseFailed err)
    Right parseOk ->
      let transformed = transformA parseOk
          bs = maybe bs0 (BS8.pack . show . pretty @b . coerce @a @b) transformed
      in  EditOk bs

-- | Build a @[FieldLine Position]@ modification function given a function @a -> a@, parsed as @b@.
modifyValueAtomAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> ([FieldLine (WithComments Position)] -> EditResult [FieldLine (WithComments Position)])
modifyValueAtomAla transformA fls0 =
  let comments = foldMap extractComments fls0
      fls = fmap removeComments fls0
  in  case joinFieldLines <$> NE.nonEmpty fls of
      Nothing -> EditUnchanged fls0
      Just (FieldLine ann0 bs0) ->
        let bsResult = modifyValueAtomBSAla @b @a transformA bs0
         in bsResult <&> \bs ->
              interleaveComments (splitFieldLines (FieldLine ann0 bs)) comments

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
--   Will parse and print with @b@.
modifyValueListBS
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Pretty b
     , Parsec (List sep (Located b) (Located a))
     )
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> (BS.ByteString -> EditResult BS.ByteString)
modifyValueListBS transformA bs0 =
  let parsecWithLeadingSpaces = liftParsec P.spaces *> parsec @(List sep (Located b) (Located a))
      parsed =
        fmap (coerce @_ @[Located a])
          . runParsecParser parsecWithLeadingSpaces "<modifyValueListBS>"
          . fieldLineStreamFromBS
          $ bs0
  in  case parsed of
    Left err -> EditErr (ParseFailed err)
    Right ok ->
      let transformed :: [Located (Maybe a)]
          transformed  = (map . fmap) transformA ok

          -- From back to front (avoid drifting), generate a list of (source range, replacement string)
          editsWithinList =
            sortBySrcSpanDes transformed >>= \case
              MkLocated _ Nothing -> []
              MkLocated spn (Just newItem) -> [(spn, BS8.pack $ show $ pretty @b $ coerce @a @b newItem)]

          performEditsWithinList = foldl' go
            where
              -- If the original snippet spans more than one line, we append the vertical spacing back by counting lines.
              -- This is because each Parsec instance eats the trailing spaces.
              go oldBS (spn, bs) = substituteSubBSAt spn oldBS (appendNewLines spn bs)

          printed = performEditsWithinList bs0 editsWithinList
       in EditOk printed

-- | Build a @[FieldLine Position]@ modification function given a function @a -> Maybe a@, parsed as @List sep b a@.
modifyValueList
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Pretty b
     , Parsec (List sep (Located b) (Located a))
     )
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> ([FieldLine (WithComments Position)] -> EditResult [FieldLine (WithComments Position)])
modifyValueList transformA fls0 =
  let comments = foldMap extractComments fls0
      fls = fmap removeComments fls0
  in  case joinFieldLines <$> NE.nonEmpty fls of
    Nothing -> EditUnchanged fls0
    Just (FieldLine ann0 bs0) ->
      let bsResult = modifyValueListBS @sep @b @a transformA bs0
       in bsResult <&> \bs ->
            interleaveComments (splitFieldLines (FieldLine ann0 bs)) comments

-- TODO(leana8959): make this partial
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
