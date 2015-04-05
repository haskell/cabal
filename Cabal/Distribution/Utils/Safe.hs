module Distribution.Utils.Safe where

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_ : tl) = Just tl

