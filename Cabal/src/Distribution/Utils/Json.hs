{-# LANGUAGE OverloadedStrings #-}

-- | Extremely simple JSON helper. Don't do anything too fancy with this!
module Distribution.Utils.Json
    ( Json(..)
    , (.=)
    , renderJson
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

data Json = JsonArray [Json]
          | JsonBool !Bool
          | JsonNull
          | JsonNumber !Int
          | JsonObject [(Text, Json)]
          | JsonRaw !Text
          | JsonString !Text

-- | A type to mirror 'ShowS'
type ShowT = Text -> Text

renderJson :: Json -> ShowT
renderJson (JsonArray objs)   =
  surround "[" "]" $ intercalate "," $ map renderJson objs
renderJson (JsonBool True)    = showText "true"
renderJson (JsonBool False)   = showText "false"
renderJson  JsonNull          = showText "null"
renderJson (JsonNumber n)     = showText $ Text.pack (show n)
renderJson (JsonObject attrs) =
  surround "{" "}" $ intercalate "," $ map render attrs
  where
    render (k,v) = (surround "\"" "\"" $ showText' k) . showText ":" . renderJson v
renderJson (JsonString s)     = surround "\"" "\"" $ showText' s
renderJson (JsonRaw s)        = showText s

surround :: Text -> Text -> ShowT -> ShowT
surround begin end middle = showText begin . middle . showText end

showText :: Text -> ShowT
showText = (<>)

showText' :: Text -> ShowT
showText' xs = showStringWorker xs
  where
      showStringWorker :: Text -> ShowT
      showStringWorker t =
        case Text.uncons t of
          Just ('\r', as) -> showText "\\r" . showStringWorker as
          Just ('\n', as) -> showText "\\n" . showStringWorker as
          Just ('\"', as) -> showText "\\\"" . showStringWorker as
          Just ('\\', as) -> showText "\\\\" . showStringWorker as
          Just (x,    as) -> showText (Text.singleton x) . showStringWorker as
          Nothing         -> showText ""

intercalate :: Text -> [ShowT] -> ShowT
intercalate sep = go
  where
    go []     = id
    go [x]    = x
    go (x:xs) = x . showText' sep . go xs

(.=) :: Text -> Json -> (Text, Json)
k .= v = (k, v)
