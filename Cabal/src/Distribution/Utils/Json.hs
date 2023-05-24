{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Extremely simple JSON helper. Don't do anything too fancy with this!
module Distribution.Utils.Json
  ( Json (..)
  , (.=)
  , renderJson
  ) where

import Data.ByteString.Builder
  ( Builder
  , intDec
  , stringUtf8
  , toLazyByteString
  )
import qualified Data.ByteString.Lazy as LBS
import Distribution.Compat.Prelude

data Json
  = JsonArray [Json]
  | JsonBool !Bool
  | JsonNull
  | JsonNumber !Int -- No support for Floats, Doubles just yet
  | JsonObject [(String, Json)]
  | JsonString !String
  deriving (Show)

-- | Convert a 'Json' into a 'ByteString'
renderJson :: Json -> LBS.ByteString
renderJson json = toLazyByteString (go json)
  where
    go (JsonArray objs) =
      surround "[" "]" $ mconcat $ intersperse "," $ map go objs
    go (JsonBool True) = stringUtf8 "true"
    go (JsonBool False) = stringUtf8 "false"
    go JsonNull = stringUtf8 "null"
    go (JsonNumber n) = intDec n
    go (JsonObject attrs) =
      surround "{" "}" $ mconcat $ intersperse "," $ map render attrs
      where
        render (k, v) = (surround "\"" "\"" $ stringUtf8 (escape k)) <> ":" <> go v
    go (JsonString s) = surround "\"" "\"" $ stringUtf8 (escape s)

surround :: Builder -> Builder -> Builder -> Builder
surround begin end middle = mconcat [begin, middle, end]

escape :: String -> String
escape ('\"' : xs) = "\\\"" <> escape xs
escape ('\\' : xs) = "\\\\" <> escape xs
escape ('\b' : xs) = "\\b" <> escape xs
escape ('\f' : xs) = "\\f" <> escape xs
escape ('\n' : xs) = "\\n" <> escape xs
escape ('\r' : xs) = "\\r" <> escape xs
escape ('\t' : xs) = "\\t" <> escape xs
escape (x : xs) = x : escape xs
escape [] = mempty

-- | A shorthand for building up 'JsonObject's
-- >>> JsonObject [ "a" .= JsonNumber 42, "b" .= JsonBool True ]
-- JsonObject [("a",JsonNumber 42),("b",JsonBool True)]
(.=) :: String -> Json -> (String, Json)
k .= v = (k, v)
