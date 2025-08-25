module MyLib (someFunc) where

import Data.Time.Clock

someFunc :: IO ()
someFunc = print =<< getCurrentTime
