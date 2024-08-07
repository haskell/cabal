{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- I suppose this is currently more of an exact-ish print
-- anything that makes it warn for example is neglected.
module Distribution.PackageDescription.ExactPrint
  ( exactPrint,
  )
where

import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as Text
import Distribution.Fields.Field (FieldName)
import Distribution.Fields.Pretty
import Distribution.PackageDescription (specVersion)
import Distribution.PackageDescription.PrettyPrint
import Distribution.Parsec.Position
import Distribution.Types.GenericPackageDescription
import Text.PrettyPrint (Doc, ($$), ($+$), (<+>))
import qualified Text.PrettyPrint as PP
import qualified Data.Text as Text

exactPrint :: GenericPackageDescription -> Text
exactPrint package = foldExactly (exactPrintMeta package) fields
  where
    fields :: [PrettyField ()]
    fields = ppGenericPackageDescription (specVersion (packageDescription (package))) package

data ExactMetaField = ExactMetaField {position :: Position, text :: Text}
  deriving Show

-- | an exact node is either some existing cabal field, or other stuff like whitespace or comments
data ExactNode
  = ExactPretty (PrettyField (Maybe ExactPosition))
  | ExactMeta ExactMetaField

commentsToMeta :: ExactPrintMeta -> [ExactMetaField]
commentsToMeta package = uncurry ExactMetaField  <$> Map.toList (exactComments package)

foldExactly :: ExactPrintMeta -> [PrettyField ()] -> Text
foldExactly meta' pretty =
  pack $ PP.render $ currentDoc $ renderLines emptyState positioned
  where
    positioned :: [ExactNode]
    positioned =
      sortFields $
        (ExactMeta <$> commentsToMeta meta') <>
        (fmap ExactPretty $
          attachPositions [] (exactPositions meta') pretty)

data RenderState = MkRenderState
  { currentPosition :: Position,
    currentDoc :: Doc
  }

emptyState :: RenderState
emptyState =
  MkRenderState
    { currentPosition = Position 1 1,
      currentDoc = mempty
    }

renderLines ::
  RenderState ->
  -- | assuming the lines are sorted on exact position
  [ExactNode] ->
  RenderState
renderLines state' fields =
  foldr renderLine state' fields

renderLine :: ExactNode -> RenderState -> RenderState
renderLine field previous =
  case field of
    (ExactPretty prettyField) -> renderPrettyLine prettyField previous
    (ExactMeta ExactMetaField{..}) ->
      let
        currentPos = currentPosition previous
        Position rows columns = position `difference` currentPos

        ppDocs = PP.text (Text.unpack text)

        out = spaceOutput rows $ PP.nest columns ppDocs

        docLines :: Int
        docLines = (length $ lines $ PP.render ppDocs)

        newPosition = retManyPos (docLines + 1) $ currentPosition previous
      in
      MkRenderState
          { currentDoc = currentDoc previous $$ out,
            currentPosition = newPosition
          }


renderPrettyLine :: PrettyField (Maybe ExactPosition) -> RenderState -> RenderState
renderPrettyLine field (previous@MkRenderState {..}) = case field of
  PrettyField mAnn name' doc ->
    let newPosition = retManyPos docLines $ case mAnn of
          Just position -> (namePosition position)
          Nothing -> currentPosition

        docLines :: Int
        docLines = (length $ lines $ PP.render doc)
     in MkRenderState
          { currentDoc = currentDoc $$ renderWithPositionAdjustment mAnn currentPosition ((decodeFieldname name') <> ":") [doc],
            currentPosition = newPosition
          }
  PrettySection mAnn name' ppDocs sectionFields ->
    let newPosition = retManyPos docLines $ case mAnn of
          Just position -> (namePosition position)
          Nothing -> currentPosition

        docLines :: Int
        docLines = (length $ lines $ PP.render $ fold ppDocs)

        result =
          MkRenderState
            { currentDoc = currentDoc $$ renderWithPositionAdjustment mAnn currentPosition (decodeFieldname name') ppDocs,
              currentPosition = newPosition
            }
     in renderLines result $ sortFields $ fmap ExactPretty $ sectionFields
  PrettyEmpty -> previous

decodeFieldname :: FieldName -> String
decodeFieldname = unpack . Text.decodeUtf8

spaceOutput :: Int -> Doc -> Doc
spaceOutput rows output =
  if rows < 0
    then -- this is a failure mode
    -- error ("unexpected empty negative rows" <> show (rows))
      output
    else -- <+> "--" <+> PP.text (show (("rows=", rows, "columns=", columns), mAnn, ("current=", current), docLines )) -- DEBUG

      let spacing :: Doc
          spacing = foldr ($+$) mempty ("" <$ [1 .. rows])
       in spacing $$ output

renderWithPositionAdjustment :: (Maybe ExactPosition) -> Position -> String -> [Doc] -> Doc
renderWithPositionAdjustment mAnn current fieldName doc =
    spaceOutput rows output
  where
    output :: Doc
    output =
      ( PP.nest
          columns
          (PP.text fieldName)
          <> ((PP.hsep ("" <$ [1 .. offset])) <> fold doc)
      )

    res@(Position rows columns) = case mAnn of
      Just position -> (namePosition position) `difference` current
      Nothing -> zeroPos

    arguments :: [Position]
    arguments = foldMap argumentPosition mAnn

    offset :: Int
    offset =
      ( case arguments of
          ((Position _ cols) : _) -> cols
          [] -> 0
      )
        - length fieldName
        - columns

-- pp randomly changes ordering, this undoes that
sortFields :: [ExactNode] -> [ExactNode]
sortFields = reverse . sortOn (exactFieldPosition)

exactFieldPosition :: ExactNode -> Maybe ExactPosition
exactFieldPosition = \case
  (ExactPretty pretty) -> join $ prettyFieldAnn pretty
  (ExactMeta meta) -> Just (ExactPosition {namePosition = position meta, argumentPosition = []})

-- .

attachPositions :: [NameSpace] -> Map [NameSpace] ExactPosition -> [PrettyField ()] -> [PrettyField (Maybe ExactPosition)]
attachPositions previous positionLookup = map (annotatePositions previous positionLookup)

annotatePositions :: [NameSpace] -> Map [NameSpace] ExactPosition -> PrettyField () -> PrettyField (Maybe ExactPosition)
annotatePositions previous positionLookup field' = case field' of
  PrettyField _ann name' doc ->
    PrettyField (Map.lookup nameSpace positionLookup) name' doc
  PrettySection _ann name' ppDoc sectionFields ->
    PrettySection (Map.lookup nameSpace positionLookup) name' ppDoc (attachPositions nameSpace positionLookup sectionFields)
  PrettyEmpty -> PrettyEmpty
  where
    nameSpace = previous <> toNameSpace field'

toNameSpace :: PrettyField () -> [NameSpace]
toNameSpace = \case
  PrettyField _ann name' doc ->
    [NameSpace {nameSpaceName = name', nameSpaceSectionArgs = []}]
  PrettySection _ann name' ppDoc sectionFields ->
    [NameSpace {nameSpaceName = name', nameSpaceSectionArgs = fmap docToBs ppDoc}]
  PrettyEmpty -> []

docToBs :: Doc -> ByteString
docToBs = encodeUtf8 . pack . PP.render -- I guess we just hope this is the same
