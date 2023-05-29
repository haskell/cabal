module MyLib (someFunc) where

import Control.Concurrent.Async

someFunc :: IO (Async ())
someFunc = async (return ())
