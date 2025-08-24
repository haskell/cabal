module MyLib (someFunc) where

import qualified Data.Text as T

someFunc :: IO ()
someFunc = print . T.unpack . T.pack $ "Hello, World!"
