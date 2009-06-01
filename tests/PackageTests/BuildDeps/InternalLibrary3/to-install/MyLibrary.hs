module MyLibrary where

import qualified Data.ByteString.Char8 as C
import System.Time

myLibFunc :: IO ()
myLibFunc = do
    getClockTime
    let text = "myLibFunc installed"
    C.putStrLn $ C.pack text
