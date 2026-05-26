{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Parsec.FieldLineStream
  ( FieldLineStream (..)
  , FlsAnn (..)
  , FlsAnnToken (..)
  , StreamBody (..)
  , fieldLineStreamFromString
  , fieldLineStreamPosition
  , fieldLineStreamFromBS
  , fieldLineStreamEnd
  ) where

import Data.ByteString (ByteString)
import Distribution.Compat.Prelude
import Distribution.Parsec.Position
import Distribution.Utils.Generic (toUTF8BS)
import Prelude ()

import Distribution.Fields.Field (Comment (..), WithComments (..))

import qualified Data.ByteString as BS
import Data.Text.Internal.Encoding.Utf8
import qualified Text.Parsec as Parsec

import Debug.Pretty.Simple

data StreamBody
  = SBComment {-# UNPACK #-} !ByteString
  | SBFieldLine {-# UNPACK #-} !ByteString
  deriving (Show)

-- | This is essentially a lazy bytestring, but chunks are glued with newline @\'\\n\'@.
data FieldLineStream
  = FLSLast {-# UNPACK #-} !StreamBody !Position
  | FLSCons {-# UNPACK #-} !StreamBody !Position FieldLineStream
  deriving (Show)

-- | Isomorphic to FieldLineStream, for prototyping the Stream instance.
newtype FlsAnn = FlsAnn { unFlsAnn :: FieldLineStream }
  deriving (Show)

data FlsAnnToken
  -- | Comment is retrieved as a single token
  = FlsAnnTComment {-# UNPACK #-} !ByteString
  -- | Normal fieldline is taken as a char
  | FlsAnnTChar {-# UNPACK #-} !Char
    deriving (Show)

fieldLineStreamPosition :: FieldLineStream -> Position
fieldLineStreamPosition (FLSLast _ pos) = pos
fieldLineStreamPosition (FLSCons _ pos _) = pos

fieldLineStreamEnd :: Position -> FieldLineStream
fieldLineStreamEnd = FLSLast (SBFieldLine mempty)

-- | Convert 'String' to 'FieldLineStream'.
--
-- /Note:/ inefficient!
fieldLineStreamFromString :: String -> FieldLineStream
fieldLineStreamFromString = flip FLSLast (Position 1 1) . SBFieldLine . toUTF8BS

fieldLineStreamFromBS :: ByteString -> FieldLineStream
fieldLineStreamFromBS = flip FLSLast (Position 1 1) . SBFieldLine

instance Monad m => Parsec.Stream FieldLineStream m Char where
  uncons = flsUncons

flsUncons :: Monad m => FieldLineStream -> m (Maybe (Char, FieldLineStream))
flsUncons (FLSLast (SBFieldLine bs) pos) = return $ case BS.uncons bs of
  Nothing -> Nothing
  Just (c, bs') -> Just (unconsChar c bs' (\bs'' -> FLSLast (SBFieldLine bs'') pos) (fieldLineStreamEnd pos))
flsUncons (FLSCons (SBFieldLine bs) pos s) = return $ case BS.uncons bs of
  -- as lines are glued with '\n', we return '\n' here!
  Nothing -> Just ('\n', s)
  Just (c, bs') -> Just (unconsChar c bs' (\bs'' -> FLSCons (SBFieldLine bs'') pos s) s)

-- flsUncons _ = error "Got comment"

flsUncons (FLSLast (SBComment cmt) pos) =
  (\x -> trace ("got comment (last) = " <> show cmt <> "\n") x)
  $ return Nothing
flsUncons (FLSCons (SBComment cmt) pos s) =
  (\x -> trace ("got comment (cons) = " <> show cmt <> "\n") x)
  flsUncons s


-- Newlines are consumed but never produced by the Field Parser.
-- We insert them back at each cons constructor consumed via 'Text.Parsec.uncons'.
--
-- This stream is consumed by the ParsecParser
instance Monad m => Parsec.Stream FlsAnn m FlsAnnToken where
  -- FlsAnn -> m ( Maybe (FlsAnnToken, FlsAnn) )
  --
  uncons (unFlsAnn->FLSLast (SBFieldLine bs) pos) = return $ case BS.uncons bs of
    Nothing -> Nothing
    Just (c, bs') ->
      let (c', bs''') = unconsChar c bs' (\bs'' -> FlsAnn $ FLSLast (SBFieldLine bs'') pos) (FlsAnn $ fieldLineStreamEnd pos)
      in  Just (FlsAnnTChar c', bs''')
  uncons (unFlsAnn->FLSCons (SBFieldLine bs) pos s) = return $ case BS.uncons bs of
    Nothing -> Just (FlsAnnTChar '\n', FlsAnn s)
    Just (c, bs') ->
      let (c', bs''') = unconsChar c bs' (\bs'' -> FlsAnn $ FLSCons (SBFieldLine bs'') pos s) (FlsAnn s)
      in  Just (FlsAnnTChar c', bs''')

  uncons (unFlsAnn->FLSLast (SBComment cmt) pos) = return $
    Just (FlsAnnTComment cmt, FlsAnn $ fieldLineStreamEnd pos)
  uncons (unFlsAnn->FLSCons (SBComment cmt) pos s) = return $
    Just (FlsAnnTComment cmt, FlsAnn $ s)


unconsChar :: forall a. Word8 -> ByteString -> (ByteString -> a) -> a -> (Char, a)
unconsChar c0 bs0 f next = go (utf8DecodeStart c0) bs0
  where
    go decoderResult bs = case decoderResult of
      Accept ch -> (ch, f bs)
      Reject -> (replacementChar, f bs)
      Incomplete state codePoint -> case BS.uncons bs of
        Nothing -> (replacementChar, next)
        Just (w, bs') -> go (utf8DecodeContinue w state codePoint) bs'

replacementChar :: Char
replacementChar = '\xfffd'
