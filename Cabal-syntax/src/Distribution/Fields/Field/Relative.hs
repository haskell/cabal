-- | We define an alternative algebra on Fields with relative positioning.
--   This will help the modification, abstracting away the positing shifting.
module Distribution.Fields.Field.Relative where

import Control.Monad.State (StateT, gets, modify, runStateT)
import Distribution.Fields.Field
import Distribution.Parsec.Position

import Data.Function ((&))
import Data.Kind
import qualified Distribution.Compat.Lens as L
import qualified Distribution.Parsec.Position.Lens as L

type RelativeT (m :: Type -> Type) = StateT Position m

fromRelativeFields :: Position -> RelativeT m [Field ann] -> m ([Field ann], Position)
fromRelativeFields = flip runStateT

toRelativeFields :: Monad m => [Field (WithComments Position)] -> RelativeT m [Field (WithComments Position)]
toRelativeFields = traverse toRelativeField

-- | Expects the fields to be sorted in ascending order by 'Position'.
toRelativeField :: Monad m => Field (WithComments Position) -> RelativeT m (Field (WithComments Position))
toRelativeField (Field colonPos fname fls) = do
  fname' <- relativizeName fname
  colonPos' <- toRelativePosition colonPos
  fls' <- toRelativeFieldLines fls
  pure (Field colonPos' fname' fls')
toRelativeField (Section sname sargs fs) = do
  sname' <- relativizeName sname
  sargs' <- toRelativeSectionArgs sargs
  fs' <- toRelativeFields fs
  pure (Section sname' sargs' fs')

toRelativeAnn :: Monad m => WithComments Position -> RelativeT m (WithComments Position)
toRelativeAnn (WithComments cmts p) =
  let (pre, post) = partitionCommentsAt p cmts
   in do
        pre' <- toRelativeComments pre
        p' <- toRelativePosition p
        post' <- toRelativeComments post
        pure (WithComments (pre' <> post') p')

toRelativeComments :: Monad m => [Comment Position] -> RelativeT m [Comment Position]
toRelativeComments = traverse toRelativeComment

toRelativeComment :: Monad m => Comment Position -> RelativeT m (Comment Position)
toRelativeComment (Comment bs p) = Comment bs <$> toRelativePosition p

toRelativePosition :: Monad m => Position -> RelativeT m Position
toRelativePosition p = gets $ \p0@(Position row0 col0) ->
  if L.view L.positionRow p == L.view L.positionRow p0
    then p & L.over L.positionCol (\col -> col0 - col)
    else p & L.over L.positionRow (\row -> row0 - row)

partitionCommentsAt :: Position -> [Comment Position] -> ([Comment Position], [Comment Position])
partitionCommentsAt p0 = span (\(Comment _ p) -> p <= p0)

relativizeName :: Monad m => Name (WithComments Position) -> RelativeT m (Name (WithComments Position))
relativizeName (Name ann name) = Name <$> toRelativeAnn ann <*> pure name

toRelativeFieldLines :: Monad m => [FieldLine (WithComments Position)] -> RelativeT m [FieldLine (WithComments Position)]
toRelativeFieldLines = traverse toRelativeFieldLine

toRelativeFieldLine :: Monad m => FieldLine (WithComments Position) -> RelativeT m (FieldLine (WithComments Position))
toRelativeFieldLine (FieldLine ann bs) = FieldLine <$> toRelativeAnn ann <*> pure bs

toRelativeSectionArgs :: Monad m => [SectionArg (WithComments Position)] -> RelativeT m [SectionArg (WithComments Position)]
toRelativeSectionArgs = traverse toRelativeSectionArg

toRelativeSectionArg :: Monad m => SectionArg (WithComments Position) -> RelativeT m (SectionArg (WithComments Position))
toRelativeSectionArg (SecArgName ann bs) = SecArgName <$> toRelativeAnn ann <*> pure bs
toRelativeSectionArg (SecArgStr ann bs) = SecArgStr <$> toRelativeAnn ann <*> pure bs
toRelativeSectionArg (SecArgOther ann bs) = SecArgOther <$> toRelativeAnn ann <*> pure bs

modifyRow :: Monad m => Int -> RelativeT m ()
modifyRow n = modify $ \(Position row col) -> Position (row + n) col

modifyCol :: Monad m => Int -> RelativeT m ()
modifyCol n = modify $ \(Position row col) -> Position row (col + n)

-- TODO(leana8959): Implement mkField primitives.
-- It should be possible to build simple fieldlines with strictly incrementing positions.
-- By putting that into a context, it will automatically be relative.
-- This will improve the 'addField' combinator.
