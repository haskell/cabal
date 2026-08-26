{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Fields.Transform
  ( Edit (..)
  , EditError (..)
  , EditResult (..)
  , MatchField
  , MatchSection

    -- * Addition
  , AddConfig (..)
  , addField
  , addSection

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

    -- * Typed 'FieldLine' modification functions
    -- $editing-fieldlines
  , modifyValueAtomBSAla
  , modifyValueAtomAla
  , modifyValueListBS
  , modifyValueList
  , prependValueListBS
  , prependValueList
  -- TODO: move to an internal module

    -- * Internal
  , substituteSubBSAt
  , splitBSAtPosition
  )
where

import qualified Text.Parsec as P

import Distribution.FieldGrammar.Parsec
  ( extractComments
  , interleaveComments
  , joinFieldLines
  , removeComments
  , splitFieldLines
  )
import Distribution.Fields.Field
import Distribution.Parsec.Position
import Distribution.Pretty
import Distribution.Utils.Generic

import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce
import Data.Functor ((<&>))
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import Distribution.Annotation
import Distribution.FieldGrammar.Newtypes
import Distribution.Fields.ConfVar
import Distribution.Parsec
import Distribution.Parsec.FieldLineStream
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import GHC.Generics

import Distribution.CabalSpecVersion
import qualified Distribution.Compat.Lens as L
import qualified Distribution.Fields.Field.Lens as L ()
import qualified Distribution.Parsec.Position.Lens as L

-- | Reified function to facilitate chaining
--   This can't be a functor (so there's no applicative nor alternative), I think there are better ways to do this.
newtype Edit a = Edit {runEdit :: CabalSpecVersion -> a -> EditResult a}

data EditResult a
  = EditOk a
  | EditUnchanged a
  | -- | a case where you might want to fallback to other edits, because apparently what you did did nothing.
    EditErr EditError
  deriving (Eq, Functor, Generic)

-- TODO(leana8959): Add a label to indicate what didn't match, or what matched and didn't change.
-- Define what is a match: when the inner thing doesn't match because there are not inner thing, what we can't rely on it
-- to tell us its label.
-- Is this where continuation comes in handy.
--
-- Maybe we can put this in the matching function's signature?
-- But then we can't retrieve it.

-- TODO(leana8959): implement a label mechanism
data EditError
  = ExpectChanges
  | ParseFailed P.ParseError
  deriving (Eq, Generic)

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
    -- Failure takes precedence.
    _ (EditErr err) -> EditErr err
    (EditErr err) _ -> EditErr err

instance Monad EditResult where
  (EditOk x) >>= f = f x
  (EditUnchanged u) >>= f = f u
  (EditErr err) >>= _ = EditErr err

-- 'EditResult' doesn't have an 'Alternative' instance because empty doesn't make sense.
-- Either we have to conjure up a 'EditUnchanged' out of thin air;
-- or we need to fail with empty by using 'EditErr'.
orFallback' :: EditResult a -> EditResult a -> EditResult a
orFallback' = liftA2 (const id)

data AddConfig = AddStart | AddEnd

type MatchField ann = Name ann -> [FieldLine ann] -> Bool
type MatchSection ann = Name ann -> [SectionArg ann] -> [Field ann] -> Bool

addField
  :: AddConfig
  -> Name ()
  -> [BS.ByteString]
  -- ^ Comments to the 'Name'
  -> [FieldLine ()]
  -> [BS.ByteString]
  -- ^ Comments to the 'FieldLine's
  -> Edit [Field (WithComments Position)]
addField ac name nameCmts fls flsCmts = Edit $ \_ -> case ac of
  AddStart -> \case
    [] -> EditOk [fst (mkFieldAt' onePos)]
    fs@(f : _) ->
      let (newField, newFieldHeight) = mkFieldAt' (L.view L.position (fieldAnn f))
          fs' = offsetFieldRow newFieldHeight <$> fs
       in EditOk (newField : fs')
  AddEnd ->
    let go [] = EditOk [fst (mkFieldAt' onePos)]
        go [f] =
          let (newField, _) = mkFieldAt' (afterFieldEndPosition f)
           in EditOk [f, newField]
        go (f : fs) = (f :) <$> go fs
     in go
  where
    mkFieldAt' = mkFieldAt name nameCmts fls flsCmts

addSection
  :: AddConfig
  -> Name ()
  -> [SectionArg ()]
  -> [BS.ByteString]
  -- ^ Comments to the 'Name'
  -> Edit [Field (WithComments Position)]
  -- ^ Nested modifications
  -> Edit [Field (WithComments Position)]
addSection ac name args nameCmts inner = Edit $ \spec ->
  let modifySectionFields (Section n as fs) = Section n as <$> runEdit inner spec fs
      modifySectionFields x = EditUnchanged x

      mkSectionAt' = mkSectionAt name args nameCmts
   in case ac of
        AddStart -> \case
          [] -> EditOk [mkSectionAt' onePos]
          fs@(f : _) ->
            let newSectionResult = modifySectionFields $ mkSectionAt' (L.view L.position (fieldAnn f))
             in newSectionResult <&> \newSection ->
                  let newSectionHeight = let (start, end) = fieldRowRange newSection in end - start
                      fs' = offsetFieldRow newSectionHeight <$> fs
                   in (newSection : fs')
        AddEnd ->
          let go [] = EditOk [mkSectionAt' onePos]
              go [f] =
                let newSectionResult = modifySectionFields $ mkSectionAt' (afterFieldEndPosition f)
                 in newSectionResult <&> \newSection -> [f, newSection]
              go (f : fs) = (f :) <$> go fs
           in go

mkCommentAtPosition :: Position -> BS.ByteString -> Comment Position
mkCommentAtPosition (Position row col) bs = Comment (BS8.replicate col ' ' <> "-- " <> bs) (Position row col)

mkSectionAt
  :: Name ()
  -> [SectionArg ()]
  -> [BS.ByteString]
  -- ^ Comments to the 'Name'
  -> Position
  -> Field (WithComments Position)
mkSectionAt name sargs nameCmts pos =
  let
    -- All content is aligned to this column.
    col0 = positionCol pos

    cmtsAboveField = zipWith (\bs row -> mkCommentAtPosition (Position row col0) bs) nameCmts [positionRow pos ..]

    namePosition = maybe pos (\(Comment _ p) -> retPos p) (safeLast cmtsAboveField)
    nameWithAnn = WithComments cmtsAboveField namePosition <$ name

    sargsRow = positionRow namePosition
    sargsWithAnn = reverse $ snd $ foldl go state0 sargs
      where
        state0 =
          ( Position sargsRow (BS8.length (getName name))
          , []
          )
        go (lastPos, acc) arg =
          let thisPosStart = incPos 1 lastPos
              thisPosEnd = incPos argLength thisPosStart
              argLength = BS8.length (sectionArgBS arg)
           in ( thisPosEnd
              , (WithComments mempty thisPosEnd <$ arg) : acc
              )

    newSection = Section nameWithAnn sargsWithAnn []
   in
    newSection

-- TODO(leana8959): rewrite this with a state monad

-- | Create a new field at a position, along with its height.
--   Comments are plain strings without @--@ prefix.
mkFieldAt
  :: Name ()
  -> [BS.ByteString]
  -- ^ Comments to the 'Name'
  -> [FieldLine ()]
  -> [BS.ByteString]
  -- ^ Comments to the 'FieldLine's
  -> Position
  -> (Field (WithComments Position), Int)
mkFieldAt name nameCmts fls flsCmts pos =
  let
    -- All content is aligned to this column.
    col0 = positionCol pos
    -- TODO(leana8959): read indentation from the config
    indentedCol = col0 + 2

    -- If there are no body, we attach the fieldlines's comments to the field.
    cmtsAboveFieldBS = if null fls then nameCmts <> flsCmts else nameCmts
    cmtsAboveField = zipWith (\bs row -> mkCommentAtPosition (Position row col0) bs) cmtsAboveFieldBS [positionRow pos ..]

    namePosition = maybe pos (\(Comment _ p) -> retPos p) (safeLast cmtsAboveField)
    nameWithAnn = WithComments cmtsAboveField namePosition <$ name

    colonPos = incPos (BS8.length (getName name)) namePosition

    cmtsAboveFls = zipWith (\bs row -> mkCommentAtPosition (Position row indentedCol) bs) flsCmts [positionRow pos ..]
    -- If there are no comments, print on the same line after the colon.
    -- Otherwise after a newline, we list out all fieldlines.
    fieldLineStartPos = maybe (incPos 2 colonPos) (\(Comment _ p) -> retPos p) (safeLast cmtsAboveFls)
    (flsWithAnn, flsHeight) = case fls of
      [] -> ([], 0)
      -- Singleline
      [fl] ->
        let fl' = WithComments cmtsAboveFls fieldLineStartPos <$ fl
         in ([fl'], 0)
      -- Multiline
      fls' ->
        let fieldLineFollowingPos = retPos fieldLineStartPos
            fls'' :: [FieldLine (WithComments Position)]
            fls'' = zipWith (\l row -> WithComments mempty (Position row indentedCol) <$ l) fls' [positionRow fieldLineFollowingPos ..]
         in (fls'', length cmtsAboveFls + 1 + length fls'')

    field = Field colonPos nameWithAnn flsWithAnn
    totalHeight =
      length cmtsAboveField
        + 1 -- field name
        + flsHeight -- inner comments and fieldlines
   in
    (field, totalHeight)

data RemoveConfig = RemoveAll | RemoveFirst

removeField
  :: RemoveConfig
  -> MatchField (WithComments Position)
  -> Edit [Field (WithComments Position)]
removeField rc match = Edit $ \_ -> case rc of
  RemoveAll -> mkEditResult <*> filter p
  RemoveFirst -> mkEditResult <*> filterOne p
  where
    p (Field _ name fls) = not (match name fls)
    p _ = True

data ModifyConfig = ModifyFirst | ModifyLast

-- Different modification strategies
modifyField
  :: ModifyConfig
  -> MatchField (WithComments Position)
  -- ^ Match a given field
  -> (CabalSpecVersion -> [FieldLine (WithComments Position)] -> EditResult [FieldLine (WithComments Position)])
  -> Edit [Field (WithComments Position)]
modifyField mc match modifyFieldLines = Edit $ \spec ->
  let doModify (Field colonPos fname fls) | match fname fls = Field colonPos fname <$> fls'
        where
          fls' = modifyFieldLines spec fls
      doModify x = EditUnchanged x

      doShift (old, new) fd =
        let (_, oldEnd) = fieldRowRange old
            (_, newEnd) = fieldRowRange new
            lineShift = (newEnd - oldEnd) `max` 0
         in offsetFieldRow lineShift fd
   in case mc of
        ModifyFirst -> \fs -> mapFirstThen doModify doShift fs
        ModifyLast -> mapLast doModify

modifySection
  :: ModifyConfig
  -> MatchSection (WithComments Position)
  -- ^ Match a given section
  -> (CabalSpecVersion -> [SectionArg (WithComments Position)] -> EditResult [SectionArg (WithComments Position)])
  -- ^ Transform the section args
  -> Edit [Field (WithComments Position)]
  -- ^ Transform inner fields
  -> Edit [Field (WithComments Position)]
modifySection mc match modifySectionArgs modifyFields = Edit $ \spec ->
  let doModify (Section sname sargs fs)
        | match sname sargs fs =
            let sargs' = modifySectionArgs spec sargs
                fs' = runEdit modifyFields spec fs
             in -- TODO(leana8959): handle the case where section args' comments
                -- push the following content down.
                Section sname <$> sargs' <*> fs'
      doModify x = EditUnchanged x

      doShift (old, new) fd =
        let (_, oldEnd) = fieldRowRange old
            (_, newEnd) = fieldRowRange new
            lineShift = (newEnd - oldEnd) `max` 0 -- ignore backjump
         in offsetFieldRow lineShift fd
   in case mc of
        ModifyFirst -> mapFirstThen doModify doShift
        ModifyLast -> mapLast doModify

-- | If up to this point things are still unchanged, make it an error and stop here.
failIfUnchanged :: Edit a -> Edit a
failIfUnchanged (Edit f) = Edit $ \spec input -> case f spec input of
  EditUnchanged{} -> EditErr ExpectChanges
  other -> other

-- | The product operator should deal with the positioning chaining
andThen :: Edit a -> Edit a -> Edit a
andThen (Edit x) (Edit y) = Edit $ \spec input -> x spec input >>= y spec

infixl 5 `andThen`

-- | Fallback if something is unchanged.
orFallback :: Edit a -> Edit a -> Edit a
orFallback (Edit x) (Edit y) = Edit $ \spec input -> x spec input `orFallback'` y spec input

infixl 4 `orFallback`

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

fieldsRowRange :: L.HasPosition ann => NonEmpty (Field ann) -> (Int, Int)
fieldsRowRange = finalize . fmap fieldRowRange
  where
    finalize ranges = (fst (NE.head ranges), snd (NE.last ranges))

-- TODO(leana8959): this doesn't measure the comments' height, we should probably do that.
fieldRowRange :: L.HasPosition ann => Field ann -> (Int, Int)
fieldRowRange (Field _colonPos fname fls) =
  let nameRow = L.view L.positionRow (nameAnn fname)
      maybeLastFieldLinePos = L.view L.positionRow . fieldLineAnn . NE.last <$> NE.nonEmpty fls
   in (nameRow, fromMaybe nameRow maybeLastFieldLinePos)
fieldRowRange (Section sname _sargs fs) =
  let nameRow = L.view L.positionRow (nameAnn sname)
      bodyEnd = snd . fieldsRowRange <$> NE.nonEmpty fs
   in (nameRow, fromMaybe nameRow bodyEnd)

-- | Compute the ending position of a field based on its range.
afterFieldEndPosition :: L.HasPosition ann => Field ann -> Position
afterFieldEndPosition f =
  let (_, endRow) = fieldRowRange f
   in Position (endRow + 1 {- next line -}) 1

offsetFieldRow :: L.HasPosition ann => Int -> Field ann -> Field ann
offsetFieldRow n = \case
  (Field colonPos fname fls) -> Field (incrementRowN colonPos) (fmap incrementRowN fname) (map (fmap incrementRowN) fls)
  (Section sname sargs fs) -> Section (fmap incrementRowN sname) (map (fmap incrementRowN) sargs) (map (fmap incrementRowN) fs)
  where
    incrementRowN :: L.HasPosition ann => ann -> ann
    incrementRowN = L.over L.positionRow (+ n)

--------------------------------------------------------------------------------
-- Editing 'FieldLine's.

-- $editing-fieldlines
--
-- This family of functions help build @ByteString -> ByteString@
-- or @[FieldLine (WithComments ann)] -> [FieldLine (WithComments ann)]@.
--
-- The annotations ('Position' and comments) will be handled automatically.

modifyValueAtomBSAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> (CabalSpecVersion -> BS.ByteString -> EditResult BS.ByteString)
modifyValueAtomBSAla transformA spec bs0 = do
  parsedOk <-
    either (EditErr . ParseFailed) (pure . coerce @b @a) $
      runParsecParser' spec (parsec @b) "<modifyValueAtomAla>" $
        fieldLineStreamFromBS bs0
  let transformed = transformA parsedOk
      bs = maybe bs0 (BS8.pack . show . prettyVersioned @b spec . coerce @a @b) transformed

  EditOk bs

-- | Build a @[FieldLine Position]@ modification function given a function @a -> a@, parsed as @b@.
modifyValueAtomAla
  :: forall (b :: Type) (a :: Type)
   . (Coercible a b, Parsec b, Pretty b)
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> (CabalSpecVersion -> [FieldLine (WithComments Position)] -> EditResult [FieldLine (WithComments Position)])
modifyValueAtomAla transformA spec fls0 =
  let comments = foldMap extractComments fls0
      fls = fmap removeComments fls0
   in case joinFieldLines <$> NE.nonEmpty fls of
        Nothing -> EditUnchanged fls0
        Just (FieldLine ann0 bs0) ->
          let bsResult = modifyValueAtomBSAla @b @a transformA spec bs0
           in bsResult <&> \bs ->
                interleaveComments (splitFieldLines (FieldLine ann0 bs)) comments

-- | The position is (1, 1)-indexed. The second element of the pair starts at the position.
--   When indexing out of upper bound, we return a newline string for the following parts.
splitBSAtPosition :: Position -> BS.ByteString -> (BS.ByteString, BS.ByteString)
splitBSAtPosition (Position row col) bs = case splitAt (row - 1) (BS8.lines bs) of
  (preLines, []) -> (BS8.unlines preLines, "\n") -- This happens to be useful.
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
  -> (CabalSpecVersion -> BS.ByteString -> EditResult BS.ByteString)
modifyValueListBS transformA spec bs0 = do
  let parsecWithLeadingSpaces = liftParsec P.spaces *> parsec @(List sep (Located b) (Located a))
  parsedOk <-
    either (EditErr . ParseFailed) (pure . coerce @_ @[Located a]) $
      runParsecParser' spec parsecWithLeadingSpaces "<modifyValueListBS>" $
        fieldLineStreamFromBS bs0

  let transformed :: [Located (Maybe a)]
      transformed = (map . fmap) transformA parsedOk

      -- From back to front (avoid drifting), generate a list of (source range, replacement string)
      editsWithinList =
        sortBySrcSpanDes transformed >>= \case
          MkLocated _ Nothing -> []
          MkLocated spn (Just newItem) -> [(spn, BS8.pack $ show $ prettyVersioned @b spec $ coerce @a @b newItem)]

      performEditsWithinList = foldl' go
        where
          -- If the original snippet spans more than one line, we append the vertical spacing back by counting lines.
          -- This is because each Parsec instance eats the trailing spaces.
          go oldBS (spn, bs) = substituteSubBSAt spn oldBS (appendNewLines spn bs)

      printed = performEditsWithinList bs0 editsWithinList

  EditOk printed

-- | Build a @[FieldLine Position]@ modification function given a function @a -> Maybe a@, parsed as @List sep b a@.
modifyValueList
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Pretty b
     , Parsec (List sep (Located b) (Located a))
     )
  => (a -> Maybe a)
  -- ^ Nothing prevents a new render.
  -> (CabalSpecVersion -> [FieldLine (WithComments Position)] -> EditResult [FieldLine (WithComments Position)])
modifyValueList transformA spec fls0 =
  let comments = foldMap extractComments fls0
      fls = fmap removeComments fls0
   in case joinFieldLines <$> NE.nonEmpty fls of
        Nothing -> EditUnchanged fls0
        Just (FieldLine pos0 bs0) ->
          let bsResult = modifyValueListBS @sep @b @a transformA spec bs0
           in bsResult <&> \bs ->
                interleaveComments (splitFieldLines (FieldLine pos0 bs)) comments

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
  -> (CabalSpecVersion -> BS.ByteString -> EditResult BS.ByteString)
prependValueListBS newItem spec bs0 = do
  let parsecWithLeadingSpaces = liftParsec P.spaces *> parsec @(List sep (Located b) (Located a))
  parseOk <-
    either (EditErr . ParseFailed) (pure . coerce @_ @[Located a]) $
      runParsecParser' spec parsecWithLeadingSpaces "<prependValueList>" $
        fieldLineStreamFromBS bs0

  let newItemBS = BS8.pack $ show $ prettyVersioned @b spec $ coerce @a @b newItem
      printed = case parseOk of
        [] -> newItemBS
        (MkLocated (SrcSpan begin _) _ : _) ->
          let sep = Proxy :: Proxy sep
              (preOldFirstBS, _) = splitBSAtPosition begin bs0
              hasLeadingSep = BS8.any (isSeparator sep) preOldFirstBS
              newSep = sepToChar sep
           in if hasLeadingSep
                then newItemBS <> bs0
                else newItemBS <> newSep <> bs0

  EditOk printed

-- | Note: when prepending into a 'Field' that has no field line, it would do nothing.
--   This is because the annotation can't be known.
--
--   To add a new 'Field', use 'addField' which handles that case specifically and generates
--   appropriate annotations.
prependValueList
  :: forall (sep :: Type) (b :: Type) (a :: Type)
   . ( Coercible a b
     , Sep sep
     , Pretty b
     , Parsec (List sep (Located b) (Located a))
     )
  => a
  -> (CabalSpecVersion -> [FieldLine (WithComments Position)] -> EditResult [FieldLine (WithComments Position)])
prependValueList newValue spec fls0 =
  let comments = foldMap extractComments fls0
      fls = fmap removeComments fls0
   in case joinFieldLines <$> NE.nonEmpty fls of
        Nothing -> EditUnchanged fls0
        Just (FieldLine ann0 bs0) ->
          let bsResult = prependValueListBS @sep @b @a newValue spec bs0
           in bsResult <&> \bs ->
                interleaveComments (splitFieldLines (FieldLine ann0 bs)) comments

modifyConditionConfVar
  :: (Condition ConfVar -> Maybe (Condition ConfVar))
  -- ^ Nothing prevents a new render.
  -> (CabalSpecVersion -> [SectionArg (WithComments Position)] -> EditResult [SectionArg (WithComments Position)])
modifyConditionConfVar _ _ [] = EditUnchanged []
modifyConditionConfVar transformA spec sargs0@(firstSarg : _) =
  let comments = foldMap extractComments sargs0
      sargs = fmap removeComments sargs0
      startPos = unComments (sectionArgAnn firstSarg)
   in do
        parseOk <-
          either (EditErr . ParseFailed) EditOk $
            P.runParser (confVarParser <* P.eof) () "<section arguments>" sargs
        case transformA parseOk of
          Just modified ->
            let printed = BS8.pack (show (ppCondition modified))
             in EditOk [SecArgName (WithComments comments startPos) printed]
          Nothing -> EditOk sargs0
