{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Parsec.FieldLineStream
  ( FieldLineStream (..)
  , FLSAnn (..)
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
newtype FLSAnn = FLSAnn { unFLSAnn :: FieldLineStream }
  deriving (Show)

data FlsAnnToken
  -- | Comment is retrieved as a single token
  = FlsAnnTComment {-# UNPACK #-} !ByteString
  -- | Normal fieldline is taken as a char
  | FLSAnnTChar {-# UNPACK #-} !Char
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


instance Monad m => Parsec.Stream FLSAnn m FlsAnnToken where
  -- FLSAnn -> m ( Maybe (FlsAnnToken, FLSAnn) )
  uncons (unFLSAnn->FLSLast (SBFieldLine bs) pos) = return $ case BS.uncons bs of
    Nothing -> Nothing
    Just (c, bs') ->
      let (c', bs''') = unconsChar c bs' (\bs'' -> FLSAnn $ FLSLast (SBFieldLine bs'') pos) (FLSAnn $ fieldLineStreamEnd pos)
      in  Just (FLSAnnTChar c', bs''')
  uncons (unFLSAnn->FLSCons (SBFieldLine bs) pos s) = return $ case BS.uncons bs of
    Nothing -> Just (FLSAnnTChar '\n', FLSAnn s)
    Just (c, bs') ->
      let (c', bs''') = unconsChar c bs' (\bs'' -> FLSAnn $ FLSCons (SBFieldLine bs'') pos s) (FLSAnn s)
      in  Just (FLSAnnTChar c', bs''')

  uncons (unFLSAnn->FLSLast (SBComment cmt) pos) = return $
    Just (FlsAnnTComment cmt, FLSAnn $ fieldLineStreamEnd pos)
  uncons (unFLSAnn->FLSCons (SBComment cmt) pos s) = return $
    Just (FlsAnnTComment cmt, FLSAnn $ s)


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
