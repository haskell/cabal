{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , FlexibleInstances #-}

module Codec.Manifest.Cabal.Internal.Version
  ( Version (..)
  , versionP
  , versionB
  ) where

import           Data.ByteString.Builder
import qualified Data.ByteString.Builder.Prim as Prim
import           Data.Char
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Int
import           Text.Parsec
import           Text.Parsec.Text.Lazy



newtype Version = Version (NonEmpty Int32)

instance Show Version where
  showsPrec _ (Version (v :| vs)) = go v vs
    where
      go x (y:zs) = shows x . showChar '.' . go y zs
      go x    []  = shows x



int10e9P :: Parser Int32
int10e9P = do
  mayc <- optionMaybe $ lookAhead anyChar
  case mayc of
    Just c
      | c <= '9' && c >= '0' ->
          if c == '0'
            then do
              _ <- anyChar
              mayd <- optionMaybe $ lookAhead anyChar
              case mayd of
                Just d
                  | d <= '9' && d >= '0' ->
                      fail "Leading zero in a version word"

                _      -> pure 0

            else do
              _ <- anyChar
              go 1 $ fromIntegral (ord c - 48)

    _      -> fail "Expected a version word"
  where
    go :: Stream s m Char => Int -> Int32 -> ParsecT s u m Int32
    go i !n = do
      mayc <- optionMaybe $ lookAhead anyChar
      case mayc of
        Just c
          | c <= '9' && c >= '0' ->
              if i >= 9
                then fail "Version word is longer than nine digits"
                else do
                  _ <- anyChar
                  go (i + 1) $ 10 * n + fromIntegral (ord c - 48)

        _      -> pure n



versionP :: Parser Version
versionP = do
  v0 <- int10e9P
  vs <- go 2 id
  pure . Version $ v0 :| vs
  where
    go :: Int -> ([Int32] -> [Int32]) -> Parser [Int32]
    go !n acc = do
      mayc <- optionMaybe $ lookAhead anyChar
      case mayc of
        Just '.'  -> do
          _ <- anyChar
          v <- int10e9P
          go (n + 1) (acc . (:) v)

        _      -> pure $ acc []



versionB :: Version -> Builder
versionB (Version (v :| vs)) = go v vs
  where
    go x (y:zs) =
         Prim.primBounded
           (Prim.int32Dec Prim.>*< Prim.liftFixedToBounded Prim.char8) (x, '.')

      <> go y zs

    go x    []  = Prim.primBounded Prim.int32Dec x
