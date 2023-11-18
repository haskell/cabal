{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- I suppose this is currently more of an exact-ish print
-- anything that makes it warn for example is neglected.
module Distribution.PackageDescription.ExactPrint
  (exactPrint
  ) where

import Distribution.Types.GenericPackageDescription
import Distribution.PackageDescription.PrettyPrint
import Data.Text(Text, pack, unpack)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint(Doc, ($+$), ($$))
import qualified Data.Map as Map
import Data.Map(Map)
import Distribution.Fields.Pretty
import qualified Data.Text.Encoding as Text
import Distribution.Fields.Field(FieldName)
import Distribution.Parsec.Position
import Data.List(sortOn)
import Control.Monad(join)
import Distribution.PackageDescription(specVersion)
import Data.Foldable(fold)

exactPrint :: GenericPackageDescription -> Text
exactPrint package = foldExactly (exactPrintMeta package) fields
  where
    fields :: [PrettyField ()]
    fields = ppGenericPackageDescription (specVersion  (packageDescription (package))) package



foldExactly :: ExactPrintMeta -> [PrettyField ()] -> Text
foldExactly meta' pretty = pack $ PP.render $ currentDoc $ renderLines emptyState positioned
  where
    positioned :: [PrettyField (Maybe ExactPosition)]
    positioned = sortFields $ attachPositions (exactPositions meta') pretty

data RenderState = MkRenderState {
     currentPosition :: Position
   , currentDoc :: Doc
  }

emptyState :: RenderState
emptyState = MkRenderState {
     currentPosition = Position 1 1
   , currentDoc = mempty
  }

renderLines ::
  RenderState ->
  [PrettyField (Maybe ExactPosition)] -> -- ^ assuming the lines are sorted on exact position
  RenderState
renderLines state' fields =
  foldr renderLine state' fields

renderLine :: PrettyField (Maybe ExactPosition) -> RenderState -> RenderState
renderLine field (previous@MkRenderState {..}) = case field of
  PrettyField mAnn name' doc ->
      let
          newPosition = case mAnn of
              Just position -> retPos (namePosition position)
              Nothing -> retPos currentPosition

      in MkRenderState {
          currentDoc = currentDoc $+$ renderWithPositionAdjustment mAnn currentPosition ((decodeFieldname name') <> ":") [doc],
          currentPosition = newPosition
        }
  PrettySection mAnn name' ppDocs sectionFields ->
    let
          newPosition = case mAnn of
              Just position -> retPos (namePosition position)
              Nothing -> retPos currentPosition

          result =  MkRenderState {
            currentDoc = currentDoc $+$ renderWithPositionAdjustment mAnn currentPosition (decodeFieldname name') ppDocs,
            currentPosition = newPosition
          }
    in renderLines result $ sortFields sectionFields

  PrettyEmpty -> previous

decodeFieldname :: FieldName -> String
decodeFieldname = unpack . Text.decodeUtf8

renderWithPositionAdjustment :: (Maybe ExactPosition) -> Position -> String -> [Doc] -> Doc
renderWithPositionAdjustment mAnn current  fieldName doc =
  if rows < 0 then error ("unexpected empty negative rows" <> show (mAnn, current, fieldName, res))
  else
    let
      spacing :: Doc
      spacing = foldr ($+$) mempty ("" <$  [1..rows])
    in
  spacing $$
  (PP.nest columns
  (PP.text fieldName ) <> ((PP.hsep ("" <$ [1..offset])) <> fold doc))
  -- <+> "--" <+> PP.text (show ((rows, columns), mAnn, current, offset)) -- DEBUG
  where
     res@(Position rows columns) = case mAnn of
              Just position -> (namePosition position) `difference` current
              Nothing -> zeroPos

     arguments :: [Position]
     arguments = foldMap argumentPosition mAnn

     offset :: Int
     offset = (case arguments of
        ((Position _ cols):_) -> cols
        [] -> 0) - length fieldName - columns

-- pp randomly changes ordering, this undoes that
sortFields :: [PrettyField (Maybe ExactPosition)] -> [PrettyField (Maybe ExactPosition)]
sortFields = reverse . sortOn (join . prettyFieldAnn)

attachPositions :: Map FieldName ExactPosition -> [PrettyField ()] -> [PrettyField (Maybe ExactPosition)]
attachPositions positionLookup = map (annotatePositions positionLookup)

annotatePositions :: Map FieldName ExactPosition -> PrettyField () -> PrettyField (Maybe ExactPosition)
annotatePositions positionLookup = \case
  PrettyField _ann name' doc ->
    PrettyField (Map.lookup name' positionLookup) name' doc
  PrettySection _ann name' ppDoc sectionFields ->
    PrettySection (Map.lookup name' positionLookup) name' ppDoc (attachPositions positionLookup sectionFields)
  PrettyEmpty -> PrettyEmpty
